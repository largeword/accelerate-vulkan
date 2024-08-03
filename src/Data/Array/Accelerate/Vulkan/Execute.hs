{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use record patterns" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# HLINT ignore "Avoid lambda" #-}

module Data.Array.Accelerate.Vulkan.Execute where

import Control.Concurrent (putMVar)
import Control.Monad (when)
import Data.Array.Accelerate.AST.Environment
  ( Env (Empty, Push),
    prj',
  )
import Data.Array.Accelerate.AST.Execute (GFunctionR (..))
import Data.Array.Accelerate.AST.Idx (Idx)
import Data.Array.Accelerate.AST.Kernel
  ( KernelArgR (..),
    OpenKernelFun (..),
  )
import Data.Array.Accelerate.AST.LeftHandSide
  ( Exists (..),
    LeftHandSide (..),
  )
import Data.Array.Accelerate.AST.Operation (ExpVars, PrimBool)
import Data.Array.Accelerate.AST.Schedule
  ( IOFun,
    Scheduled,
    reprIsBody,
  )
import Data.Array.Accelerate.AST.Schedule.Sequential
  ( ArrayInstr (..),
    GroundR (GroundRbuffer, GroundRscalar),
    GroundsR,
    PreArgs (ArgsNil, (:>:)),
    SArg (SArgBuffer, SArgScalar),
    SArgs,
    SeqSchedule (..),
    SeqScheduleFun (..),
    SequentialSchedule (..),
    Var (Var),
    expType,
  )
import Data.Array.Accelerate.Array.Buffer (Buffer (..), indexBuffer, newBuffer, unsafeFreezeBuffer, writeBuffer)
import Data.Array.Accelerate.Backend
  ( Backend (..),
    DesugarAcc,
    Execute (..),
  )
import Data.Array.Accelerate.Interpreter
  ( EvalArrayInstr (EvalArrayInstr),
    evalExp,
    toBool,
  )
import Data.Array.Accelerate.Lifetime (addFinalizer, newLifetime, touchLifetime, withLifetime)
import Data.Array.Accelerate.Pretty.Schedule (PrettySchedule)
import Data.Array.Accelerate.Representation.Elt (bytesElt)
import Data.Array.Accelerate.Representation.Shape (ShapeR (..))
import Data.Array.Accelerate.Representation.Type
  ( TupR (..),
    TypeR,
    mapTupR,
  )
import Data.Array.Accelerate.Type (IntegralType (..), NumType (..), ScalarType (..), SingleType (..))
import Data.Array.Accelerate.Vulkan.Common (VulkanArg (..), VulkanElement (..), VulkanElements, VulkanElementsIOFun, VulkanEnv, updateEnv)
import Data.Array.Accelerate.Vulkan.Kernel (VulkanKernel (..), vkGlobalResources)
import Data.Array.Accelerate.Vulkan.Operation (VulkanOp)
import Data.Array.Accelerate.Vulkan.Vulkan.Runtime (Ptr' (..), compute, copyBufferToPtr, copyDeviceDataToHost, copyScalarToPtr, createBufferInfo)
import Data.Array.Accelerate.Vulkan.Vulkan.Type (GlobalVulkanResources (..))
import Data.Bits ((.|.))
import Data.Data (type (:~:) (Refl))
import Foreign.Ptr (castPtr)
import GHC.IO (unsafePerformIO)
import Vulkan.Core10 (BufferUsageFlagBits (..), MemoryPropertyFlagBits (..), pattern WHOLE_SIZE)
import Vulkan.Zero (zero)
import VulkanMemoryAllocator (AllocationCreateFlagBits (..), destroyBuffer, flushAllocation, mapMemory, unmapMemory)
import Prelude hiding (exp)

data Vulkan where
  Vulkan :: Vulkan

instance (DesugarAcc VulkanOp, PrettySchedule SequentialSchedule) => Backend Vulkan where
  type Schedule Vulkan = SequentialSchedule
  type Kernel Vulkan = VulkanKernel

instance Execute SequentialSchedule VulkanKernel where
  executeAfunSchedule :: GFunctionR t -> SequentialSchedule VulkanKernel () (Scheduled SequentialSchedule t) -> IOFun (Scheduled SequentialSchedule t)
  executeAfunSchedule gf ss = executeVulkanElementsIOFun gf $ executeSequentialSchedule Empty ss

-- | Execute VulkanElementsIOFun
executeVulkanElementsIOFun :: GFunctionR t -> VulkanElementsIOFun (Scheduled SequentialSchedule t) -> IOFun (Scheduled SequentialSchedule t)
executeVulkanElementsIOFun (GFunctionRlam gr gf@(GFunctionRlam _ _)) vkEF =
  \value -> executeVulkanElementsIOFun gf $ vkEF $ unsafePerformIO $ liftAcc gr value True
executeVulkanElementsIOFun (GFunctionRlam gr gf@(GFunctionRbody gfBody)) vkEF
  | Refl <- reprIsBody @SequentialSchedule gfBody =
      \value -> executeVulkanElementsIOFun gf $ vkEF $ unsafePerformIO $ liftAcc gr value True
executeVulkanElementsIOFun (GFunctionRbody gr) vkEF
  | Refl <- reprIsBody @SequentialSchedule gr =
      \mvar -> do
        rtn <- vkEF
        putMVar mvar rtn

-- | Lift ground variables to VulkanElements
liftAcc :: GroundsR t -> t -> Bool -> IO (VulkanElements t)
liftAcc (TupRsingle (GroundRscalar st)) s isCopy = do
  vkBuffer <- withLifetime vkGlobalResources $ \vkGlobal -> do
    -- Allocate new Vulkan buffer
    -- Different devices have support on different combinations of flags, see https://vulkan.gpuinfo.org/listmemory.php
    -- See https://hackage.haskell.org/package/VulkanMemoryAllocator-0.11.0.1/docs/VulkanMemoryAllocator.html#t:AllocationCreateFlags
    -- and https://hackage.haskell.org/package/VulkanMemoryAllocator-0.11.0.1/docs/VulkanMemoryAllocator.html#t:MemoryUsage
    -- for more information on the flags and usage
    [vkBuffer'@(buffer, allocation, _)] <-
      createBufferInfo
        (allocator vkGlobal)
        [bytesElt (TupRsingle st)]
        -- Buffer usages
        BUFFER_USAGE_STORAGE_BUFFER_BIT
        -- Host access pattern
        ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT
        -- Memory location
        zero
        -- Memory properties
        (MEMORY_PROPERTY_DEVICE_LOCAL_BIT .|. MEMORY_PROPERTY_HOST_VISIBLE_BIT)
    if buffer /= zero
      then do
        -- Start mapping the device memory
        elemStartAddr <- mapMemory (allocator vkGlobal) allocation
        -- Data transfer
        when isCopy $ do
          copyScalarToPtr st s (Ptr' $ castPtr elemStartAddr)
          flushAllocation (allocator vkGlobal) allocation 0 WHOLE_SIZE
        -- Wrap in Lifetime
        vkBuffer'' <- newLifetime (vkBuffer', elemStartAddr)
        -- say $ T.pack "Creating scalar Vulkan buffer"
        -- Add finalizer
        addFinalizer vkBuffer'' $ do
          unmapMemory (allocator vkGlobal) allocation
          destroyBuffer (allocator vkGlobal) buffer allocation
        -- say $ T.pack "GC: Destroying scalar Vulkan buffer"
        pure vkBuffer''
      else newLifetime (vkBuffer', zero)
  touchLifetime vkGlobalResources
  pure $ TupRsingle $ Scalar' st s vkBuffer
liftAcc (TupRsingle (GroundRbuffer st)) b@(Buffer n _) isCopy = do
  vkBuffer <- withLifetime vkGlobalResources $ \vkGlobal -> do
    -- Allocate new Vulkan buffer
    [vkBuffer'@(buffer, allocation, _)] <-
      createBufferInfo
        (allocator vkGlobal)
        [bytesElt (TupRsingle st) * n]
        -- Buffer usages
        (BUFFER_USAGE_STORAGE_BUFFER_BIT .|. BUFFER_USAGE_TRANSFER_SRC_BIT .|. BUFFER_USAGE_TRANSFER_DST_BIT)
        -- Host access pattern
        ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT
        -- Memory location
        zero
        -- Memory properties
        (MEMORY_PROPERTY_DEVICE_LOCAL_BIT .|. MEMORY_PROPERTY_HOST_VISIBLE_BIT)
    if buffer /= zero
      then do
        -- Start mapping the device memory
        elemStartAddr <- mapMemory (allocator vkGlobal) allocation
        -- Data transfer
        when isCopy $ do
          copyBufferToPtr st b (Ptr' $ castPtr elemStartAddr)
          flushAllocation (allocator vkGlobal) allocation 0 WHOLE_SIZE
        -- Wrap in Lifetime
        vkBuffer'' <- newLifetime (vkBuffer', elemStartAddr)
        -- say $ T.pack "Creating Vulkan buffer"
        -- Add finalizer
        addFinalizer vkBuffer'' $ do
          unmapMemory (allocator vkGlobal) allocation
          destroyBuffer (allocator vkGlobal) buffer allocation
        -- say $ T.pack "GC: Destroying Vulkan buffer"
        pure vkBuffer''
      else newLifetime (vkBuffer', zero)
  touchLifetime vkGlobalResources
  pure $ TupRsingle $ Buffer' st b vkBuffer
liftAcc (TupRpair t1 t2) (v1, v2) isCopy = do
  v1' <- liftAcc t1 v1 isCopy
  v2' <- liftAcc t2 v2 isCopy
  pure $ TupRpair v1' v2'
liftAcc TupRunit _ _ = pure TupRunit

-- | Lift ordinary scalars to VulkanElements
liftVulkanElements :: TypeR t -> t -> IO (VulkanElements t)
liftVulkanElements TupRunit _ = pure TupRunit
liftVulkanElements (TupRsingle st) v = liftAcc (TupRsingle (GroundRscalar st)) v True
liftVulkanElements (TupRpair t1 t2) (v1, v2) = do
  v1' <- liftVulkanElements t1 v1
  v2' <- liftVulkanElements t2 v2
  pure $ TupRpair v1' v2'

-- | Return VulkanElements to ordinary IO values
returnVulkanElement :: VulkanElement a -> IO a
returnVulkanElement = copyDeviceDataToHost vkGlobalResources

returnVulkanElements :: VulkanElements a -> IO a
returnVulkanElements TupRunit = pure ()
returnVulkanElements (TupRsingle e) = returnVulkanElement e
returnVulkanElements (TupRpair l r) =
  do
    l' <- returnVulkanElements l
    r' <- returnVulkanElements r
    pure (l', r')

-- | Execute SequentialSchedule
executeSequentialSchedule :: forall env t. VulkanEnv env -> SequentialSchedule VulkanKernel env t -> VulkanElementsIOFun t
executeSequentialSchedule env (SequentialLam lhs ss@(SequentialLam _ _)) = \vkElem -> executeSequentialSchedule (updateEnv env lhs vkElem) ss
executeSequentialSchedule env (SequentialLam lhs ss@(SequentialBody _)) = \vkElem -> executeSequentialSchedule (updateEnv env lhs vkElem) ss
executeSequentialSchedule env (SequentialBody ss) = do
  vkElems <- executeSeqSchedule env ss
  returnVulkanElements vkElems

-- | Get array size from shape
getSize :: VulkanEnv env -> ShapeR sh -> ExpVars env sh -> Int
getSize _ ShapeRz _ = 1
getSize env (ShapeRsnoc shr) (TupRpair sh (TupRsingle (Var _ idx))) =
  if n <= 0
    then 0
    else getSize env shr sh * n
  where
    Scalar' _ n _ = prj' idx env
getSize _ _ _ = error "getSize: Impossible"

-- | Execute SeqSchedule
executeSeqSchedule :: VulkanEnv env -> SeqSchedule VulkanKernel env t -> IOFun (VulkanElements t)
executeSeqSchedule env (Exec _ f args) = executeKernelFun env Empty f args
executeSeqSchedule env (Return gv) = return $ mapTupR (\(Var _ idx) -> prj' idx env) gv
executeSeqSchedule env (Compute exp) =
  do
    let value = evalExp exp $ EvalArrayInstr $ \f s -> unsafePerformIO $ evalArrayInstr env f s
    liftVulkanElements (expType exp) value
executeSeqSchedule env (Alet lhs _ bnd body) =
  do
    newVars <- executeSeqSchedule env bnd
    let env' = updateEnv env lhs newVars
    executeSeqSchedule env' body
executeSeqSchedule env (Alloc shr st sh) =
  do
    -- say $ T.pack ("Alloc: " ++ show (getSize env shr sh))
    newMut <- newBuffer st (getSize env shr sh)
    let newBuf = unsafeFreezeBuffer newMut
    liftAcc (TupRsingle $ GroundRbuffer st) newBuf False
executeSeqSchedule _ (Use st _ buf) = liftAcc (TupRsingle $ GroundRbuffer st) buf True
executeSeqSchedule env (Unit (Var st idx)) =
  do
    let s = case prj' idx env of
          Scalar' _ s' _ -> s'
          Buffer' {} -> error "executeSeqSchedule: Unit impossible"
    newMut <- newBuffer st 1
    writeBuffer st newMut 0 s
    let newBuf = unsafeFreezeBuffer newMut
    liftAcc (TupRsingle $ GroundRbuffer st) newBuf True
executeSeqSchedule env (Acond (Var _ idx) tExp fExp) =
  if toBool cond
    then executeSeqSchedule env tExp
    else executeSeqSchedule env fExp
  where
    (Scalar' _ cond _) = prj' idx env
executeSeqSchedule env (Awhile _ cond body initial) = executeAwhile env cond body (mapTupR (\(Var _ idx) -> prj' idx env) initial)

-- | Evaluate ArrayInstr given an ordinary index
evalArrayInstr :: VulkanEnv env -> ArrayInstr env (s -> t) -> s -> IO t
evalArrayInstr env (Index (Var _ idx)) i =
  case prj' idx env of
    Scalar' {} -> error "evalArrayInstr: Index impossible"
    b@(Buffer' st _ _) -> do
      b' <- returnVulkanElement b
      pure $ indexBuffer st b' i
evalArrayInstr env (Parameter (Var _ idx)) _ =
  case prj' idx env of
    Scalar' _ e _ -> pure e
    Buffer' {} -> error "evalArrayInstr: Parameter impossible"

-- | Execute a while loop
executeAwhile :: VulkanEnv env -> SeqScheduleFun VulkanKernel env (t -> PrimBool) -> SeqScheduleFun VulkanKernel env (t -> t) -> VulkanElements t -> IOFun (VulkanElements t)
executeAwhile env cond@(Slam condLhs (Sbody condBody)) body@(Slam expLhs (Sbody expBody)) xs = do
  -- Check condition.
  (TupRsingle condVar) <- executeSeqSchedule (updateEnv env condLhs xs) condBody
  condVar' <- returnVulkanElement condVar
  -- If true, continue the loop.
  if toBool condVar'
    then do
      xs' <- executeSeqSchedule (updateEnv env expLhs xs) expBody
      executeAwhile env cond body xs'
    else -- If false, return values.
      return xs
executeAwhile _ _ _ _ = error "executeAwhile: Impossible"

-- | Execute KernelFun
executeKernelFun :: forall env env' f. VulkanEnv env -> Env (Idx env) env' -> OpenKernelFun VulkanKernel env' f -> SArgs env f -> IOFun (VulkanElements ())
executeKernelFun env env' (KernelFunLam (KernelArgRscalar _) f) ((SArgScalar (Var _ idx)) :>: args) =
  executeKernelFun env (Push env' idx) f args
executeKernelFun env env' (KernelFunLam (KernelArgRbuffer _ _) f) ((SArgBuffer _ (Var _ idx)) :>: args) =
  executeKernelFun env (Push env' idx) f args
executeKernelFun env env' (KernelFunBody kernel) ArgsNil = executeKernel env env' kernel

-- | Execute VulkanKernel
executeKernel :: VulkanEnv env -> Env (Idx env) env' -> VulkanKernel env' -> IOFun (VulkanElements ())
executeKernel env env' kernel =
  do
    let stInt = SingleScalarType (NumSingleType (IntegralNumType TypeInt))
    -- Convert shape indices into VulkanArg
    let sh' = map (\idx -> Exists (VulkanArg (Var (GroundRscalar stInt) idx))) (kernelShapes kernel)
    -- let lock' = case kernelLock kernel of
    --       Just (len, ini) -> Just (product $ map (\idx -> let (Scalar' _ s _) = prj' (prj' idx env') env in s) len, ini)
    --       Nothing -> Nothing
    let threads' = product $ map (\idx -> let (Scalar' _ s _) = prj' (prj' idx env') env in s) (kernelThreads kernel)
    -- say $ T.pack ("sh ++ inBuffs: " ++ show (sh' ++ inBuffs))
    -- say $ T.pack ("mutBuffs: " ++ show mutBuffs)
    -- say $ T.pack ("outBuffs: " ++ show outBuffs)
    -- say $ T.pack ("threads: " ++ show (threads env))
    -- say $ T.pack ("locks: " ++ show locks')
    withLifetime vkGlobalResources (\vkGlobal -> withLifetime (kernelResources kernel) (\vkLocalObj' -> compute vkGlobal vkLocalObj' env env' (sh' ++ kernelInArgs kernel) (kernelMutArgs kernel) (kernelOutArgs kernel) threads'))
    touchVulkanArgs env env' $ sh' ++ kernelInArgs kernel ++ kernelMutArgs kernel ++ kernelOutArgs kernel
    touchLifetime (kernelResources kernel)
    touchLifetime vkGlobalResources
    return TupRunit

-- | Touch VulkanArgs to prevent garbage collection
touchVulkanArgs :: VulkanEnv env -> Env (Idx env) env' -> [Exists (VulkanArg env')] -> IOFun ()
touchVulkanArgs _ _ [] = pure ()
touchVulkanArgs env env' (Exists (VulkanArg (Var _ idx)) : xs) = case prj' (prj' idx env') env of
  Scalar' _ _ lt -> do
    touchLifetime lt
    touchVulkanArgs env env' xs
  Buffer' _ _ lt -> do
    touchLifetime lt
    touchVulkanArgs env env' xs
