{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Array.Accelerate.Vulkan.Kernel where

import Control.DeepSeq (NFData (rnf))
import Control.Monad.State.Lazy (MonadState (get, put), State, runState)
import Data.Array.Accelerate.AST.Environment
  ( Env (..),
    PartialEnv (..),
    diffPartialEnv,
    partialUpdate,
    weakenId,
    weakenSucc,
    (:>),
  )
import Data.Array.Accelerate.AST.Idx (Idx, idxToInt)
import Data.Array.Accelerate.AST.Kernel (NoKernelMetadata, OpenKernelFun (..))
import Data.Array.Accelerate.AST.LeftHandSide (Exists (..), LeftHandSide (..))
import Data.Array.Accelerate.AST.Operation
  ( AccessGroundR,
    Arg (ArgArray, ArgFun),
    Args,
    ArrayInstr (..),
    GroundR (..),
    GroundVars,
    PreArgs (ArgsNil, (:>:)),
    PreOpenExp (..),
    PreOpenFun (Body, Lam),
    Var (Var),
  )
import Data.Array.Accelerate.AST.Partitioned (Clustered (Clustered))
import Data.Array.Accelerate.Backend
  ( IsKernel (..),
    NFData' (..),
    PrettyKernel (..),
    PrettyKernelStyle (PrettyKernelFun),
    hashOperation,
  )
import Data.Array.Accelerate.Eval (applySingletonCluster, peekSingletonCluster)
import Data.Array.Accelerate.LLVM.Compile.Cache (UID)
import Data.Array.Accelerate.Lifetime (Lifetime, addFinalizer, newLifetime, unsafeGetValue, withLifetime)
import Data.Array.Accelerate.Pretty.Exp (Adoc)
import Data.Array.Accelerate.Representation.Array
  ( ArrayR (ArrayR),
    Buffer,
    Buffers,
  )
import Data.Array.Accelerate.Representation.Elt (bytesElt)
import Data.Array.Accelerate.Representation.Shape (ShapeR (..))
import Data.Array.Accelerate.Representation.Type (TupR (..), TypeR, mapTupR, reprIsSingle)
import Data.Array.Accelerate.Type
  ( FloatingType (..),
    IntegralType (..),
    NumType (..),
    ScalarType (..),
    SingleType (..),
  )
import Data.Array.Accelerate.Vulkan.Common (VulkanArg (..), updateEnv)
import Data.Array.Accelerate.Vulkan.Compile.Atomic (compileAtomic, compileAtomicExchange)
import Data.Array.Accelerate.Vulkan.Compile.Convert (integralTypeToString, scalarTypeToString, singleTypeToString)
import Data.Array.Accelerate.Vulkan.Compile.Type
  ( AInstr (..),
    AInstrEnv,
    ExpString (..),
    ExpStringTup,
    FuncMap,
    VarCount,
    VarName (VarName),
    VarNameTup,
    convertVarName2ExpString,
  )
import Data.Array.Accelerate.Vulkan.Compile.Expression (compileStatement)
import Data.Array.Accelerate.Vulkan.Compile.Index (compileToIndex)
import Data.Array.Accelerate.Vulkan.Compile.Var (newAndBindVars)
import Data.Array.Accelerate.Vulkan.Operation (VulkanOp (..))
import Data.Array.Accelerate.Vulkan.Vulkan.Runtime
  ( compileGLSL,
    createGlobalResources,
    createLocalResources,
    destroyGlobalResources,
    destroyLocalResources,
  )
import Data.Array.Accelerate.Vulkan.Vulkan.Type (GlobalVulkanResources (..), LocalVulkanResources (..))
import qualified Data.Map.Ordered as OMap
import Data.String (IsString (fromString))
import Data.Type.Equality ((:~:) (Refl))
import Debug.Trace (trace)
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.Int (Int32)

data VulkanKernel env where
  VkKernel ::
    { kernelId :: {-# UNPACK #-} !UID,
      -- Contains the shapes of all buffers.
      kernelShapes :: [Idx env Int],
      -- Contains input buffer args.
      kernelInArgs :: [Exists (VulkanArg env)],
      -- Contains mutable buffer args.
      kernelMutArgs :: [Exists (VulkanArg env)],
      -- Contains output buffer args.
      kernelOutArgs :: [Exists (VulkanArg env)],
      -- Store pre-built local Vulkan objects.
      kernelResources :: Lifetime (LocalVulkanResources env),
      -- Store scalars to compute the number of threads
      kernelThreads :: [Idx env Int]
    } ->
    VulkanKernel env

-- | Extract each arg from args of the kernel
argBuffers :: Arg env (m sh e) -> [Exists (VulkanArg env)]
argBuffers (ArgArray _ (ArrayR _ a) _ gvb) = goBuffer a gvb
  where
    -- \| Extract each buffer from buffers
    goBuffer :: forall a env. TypeR a -> GroundVars env (Buffers a) -> [Exists (VulkanArg env)]
    goBuffer TupRunit TupRunit = []
    goBuffer (TupRsingle st) (TupRsingle buff@(Var _ _))
      | Refl <- reprIsSingle @ScalarType @a @Buffer st =
          [Exists (VulkanArg buff)]
    goBuffer (TupRpair t1 t2) (TupRpair gvb1 gvb2) =
      goBuffer t1 gvb1 ++ goBuffer t2 gvb2
    goBuffer _ _ = error "impossible"

-- | Prepare AInstrMap, given Arg and buffer names
argBuffers' :: Arg env (m sh e) -> VarNameTup e -> [(Int, (String, Exists (VulkanArg env)))]
argBuffers' (ArgArray _ (ArrayR _ a) _ gvb) buffNames = go a gvb buffNames
  where
    go :: forall env' a'. TypeR a' -> GroundVars env' (Buffers a') -> VarNameTup a' -> [(Int, (String, Exists (VulkanArg env')))]
    go TupRunit TupRunit TupRunit = []
    go (TupRsingle st) (TupRsingle buff@(Var _ idx)) (TupRsingle (VarName v))
      | Refl <- reprIsSingle @ScalarType @a' @Buffer st =
          [(idxNum, (v, Exists (VulkanArg buff)))]
      where
        idxNum = idxToInt idx
    go (TupRpair t1 t2) (TupRpair gvb1 gvb2) (TupRpair v1 v2) =
      go t1 gvb1 v1 ++ go t2 gvb2 v2
    go _ _ _ = error "impossible"

-- | Extract each dim from the shape
argDims :: Arg env (m sh e) -> [Idx env Int]
argDims (ArgArray _ (ArrayR sh _) gv _) = go sh gv
  where
    go :: ShapeR sh' -> GroundVars env' sh' -> [Idx env' Int]
    go ShapeRz _ = []
    go (ShapeRsnoc shr) (TupRpair vars (TupRsingle (Var _ idx))) =
      go shr vars ++ [idx]
    go _ _ = error "impossible"

-- | Prepare AInstrMap, given shape Arg and shape buffer names
argDims' :: Arg env (m sh e) -> VarNameTup sh -> [(Int, (String, Exists (VulkanArg env)))]
argDims' (ArgArray _ (ArrayR sh _) gv _) buffNames = go sh gv buffNames
  where
    go :: ShapeR sh' -> GroundVars env' sh' -> VarNameTup sh' -> [(Int, (String, Exists (VulkanArg env')))]
    go ShapeRz TupRunit TupRunit = []
    go (ShapeRsnoc shr) (TupRpair vars (TupRsingle (Var _ idx))) (TupRpair v1 (TupRsingle (VarName v2))) =
      go shr vars v1 ++ [(idxNum, (v2, Exists (VulkanArg (Var (GroundRscalar stInt) idx))))]
      where
        stInt = SingleScalarType (NumSingleType (IntegralNumType TypeInt))
        idxNum = idxToInt idx
    go _ _ _ = error "impossible"

-- | Create global resources for Vulkan, which will be shared among all kernels
{-# NOINLINE vkGlobalResources #-}
vkGlobalResources :: Lifetime GlobalVulkanResources
vkGlobalResources = unsafePerformIO $ do
  vkGlobal <- createGlobalResources 50
  lt <- newLifetime vkGlobal
  -- Add finalizer for GC to destroy the global resources, since it isn't managed by the GC
  -- !NOTE: When GC is triggered, the global resources will destroy
  -- !NOTE: But no guarantee that GC will be triggered before the program exits
  addFinalizer lt $ do
    -- say $ pack $ "GC: Destroying global Vulkan resources of " ++ show (inst vkGlobal)
    destroyGlobalResources vkGlobal
  -- say $ pack $ "Creating global Vulkan resources of " ++ show (inst vkGlobal)
  pure lt

instance NFData' VulkanKernel where
  rnf' :: VulkanKernel a -> ()
  rnf' (VkKernel !_ sh in' mut out fn thr) = unsafeGetValue fn `seq` rnf sh `seq` rnf in' `seq` rnf mut `seq` rnf out `seq` rnf thr

newtype VulkanKernelMetadata f = VulkanKernelMetadata {kernelArgsSize :: Int}

instance NFData' VulkanKernelMetadata where
  rnf' :: VulkanKernelMetadata a -> ()
  rnf' (VulkanKernelMetadata sz) = rnf sz

instance PrettyKernel VulkanKernel where
  prettyKernel :: PrettyKernelStyle VulkanKernel
  prettyKernel = PrettyKernelFun go
    where
      go :: OpenKernelFun VulkanKernel env t -> Adoc
      go (KernelFunLam _ f) = go f
      go (KernelFunBody kernel) = fromString $ take 16 $ show $ kernelId kernel

instance IsKernel VulkanKernel where
  type KernelOperation VulkanKernel = VulkanOp
  type KernelMetadata VulkanKernel = NoKernelMetadata

  compileKernel ::
    forall env args.
    Env AccessGroundR env ->
    Data.Array.Accelerate.AST.Partitioned.Clustered
      (KernelOperation VulkanKernel)
      args ->
    Args env args ->
    VulkanKernel env
  compileKernel _ (Clustered clst bclst) clstArgs
    | Just _ <- peekSingletonCluster peekOp clst =
        case ( bytesElt (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt)))),
               bytesElt (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord))))
             ) of
          (8, 8) -> applySingletonCluster (compileOperation uid) clst clstArgs
          _ -> error "Accelerate Vulkan backend only supports 64-bit GHC, with 64-bit Int and Word as default"
    where
      uid = hashOperation (Clustered clst bclst) clstArgs
  compileKernel _ _ clstArgs = error $ "compileKernel: Not a singleton cluster with args (" ++ show clstArgs ++ ")"

-- | Create custom data type to bypass the type checker
data WhichOp = GenerateOp | PermuteOp

instance Show WhichOp where
  show GenerateOp = "GenerateOp"
  show PermuteOp = "PermuteOp"

-- | Peek at the operation type
peekOp :: forall args. VulkanOp args -> WhichOp
peekOp VkGenerate = GenerateOp
peekOp VkPermute = PermuteOp

compileOperation :: forall env a. UID -> VulkanOp a -> Args env a -> VulkanKernel env
compileOperation
  uid
  VkGenerate
  ( ArgFun (Lam lhs (Body body))
      :>: outBuf@(ArgArray m (ArrayR sh t) gv gvb)
      :>: ArgsNil
    ) =
    -- trace (show args) $
    -- trace (show t) $
    -- trace glslCode $
      VkKernel uid outShape inBuffers [] outBuffers vkObjects threads
    where
      idxPrefix = "out"
      -- Compile the shape and index of the output buffer
      (glslShBuffers, glslFromIndex, outIndices, _, buffCount) = compileFromIdx sh idxPrefix 0
      newEnv = updateEnv Empty lhs outIndices
      -- Compile the body expression
      ((bodyStatement, bodyExp), (_, funcMap, aInstrEnv)) = runState (compileStatement newEnv body) (0, OMap.empty, PEnd)

      bufferPrefix = "out" -- Set the prefix name of the buffer
      -- Compile buffers called by ArrayInstr
      (glslAInstrBuffers, inBuffers, buffCount') = compileAInstrEnv aInstrEnv buffCount
      -- Compile the output buffer
      (glslBuffers, bufferName, _) = compileAddBufferArgs t bufferPrefix buffCount'
      -- Compile the results binding to output buffers
      bufferBindExp = compileUpdateBuffers bufferName bodyExp
      glslBindExp = flattenExpStringTup bufferBindExp

      -- Compile extra functions
      glslFunc = compileExtraFunc funcMap
      glslBoundaryCheck = compileBoundaryCheck bufferName "globalID"
      glslMain =
        "void main() { \n"
          ++ glslGlobalID
          ++ glslFromIndex
          ++ glslBoundaryCheck
          ++ bodyStatement
          ++ glslBindExp
          ++ "} \n"
      glslCode = glslVersion ++ glslExtension ++ glslGroupSize ++ glslShBuffers ++ glslAInstrBuffers ++ glslBuffers ++ glslFunc ++ glslMain

      spirvCode = unsafePerformIO $ compileGLSL glslCode
      vkObjects = unsafePerformIO $ do
        vkLocal <- withLifetime vkGlobalResources (\vkGlobal -> createLocalResources vkGlobal spirvCode Nothing)
        lt <- newLifetime vkLocal
        addFinalizer lt $ do
          -- say $ pack $ "GC: Destroying local Vulkan resources owned by kernel " ++ show uid
          withLifetime vkGlobalResources (`destroyLocalResources` vkLocal)
        -- say $ pack $ "Creating local Vulkan resources owned by kernel " ++ show uid
        pure lt

      outShape = argDims outBuf
      outBuffers = argBuffers outBuf
      threads = outShape

-- !NOTE: combLhs1 and comLhs2 correspond to the source (perm) and default (mut) values respectively
compileOperation
  uid
  VkPermute
  ( ArgFun fun@(Lam combLhs1 (Lam combLhs2 (Body combBody)))
      :>: aMut@(ArgArray m (ArrayR sh t) gv gvb)
      :>: ArgFun (Lam permLhs (Body permBody))
      :>: aIn@(ArgArray m' (ArrayR sh' t') gv' gvb')
      :>: ArgsNil
    ) =
    -- trace (show combLhs1 ++ show combLhs2) $
    -- trace ("permShape: " ++ show (length permShape)) $
    -- trace ("mutShape: " ++ show (length mutShape)) $
    -- trace ("aInstrBuffers: " ++ show aInstrBuffers) $
    -- trace ("permBuffers: " ++ show permBuffers) $
    -- trace ("mutBuffers: " ++ show mutBuffers) $
    -- trace ("allShape: " ++ show (length allShape)) $
    -- trace ("inBuffers: " ++ show inBuffers) $
    -- trace ("mutBuffers: " ++ show mutBuffers) $
    -- trace ("permShape: " ++ show (length permShape)) $
    -- trace ("mutShape: " ++ show (length mutShape)) $
    -- trace ("aInstrBuffers: " ++ show aInstrBuffers) $
    -- trace ("permBuffers: " ++ show permBuffers) $
    -- trace ("mutBuffers: " ++ show mutBuffers) $
    -- trace ("allShape: " ++ show (length allShape)) $
    -- trace ("inBuffers: " ++ show inBuffers) $
    -- trace ("mutBuffers: " ++ show mutBuffers) $
    -- trace glslCode $
      VkKernel uid allShapes inBuffers mutBuffers [] vkObjects threads
    where
      permIdxPrefix = "perm"
      permBuffPrefix = "perm"
      -- Compile source array shapes and indices
      (glslPermShBuffers, glslPermFromIndex, permIndices, _, buffCount) = compileFromIdx sh' permIdxPrefix 0
      permEnv = updateEnv Empty permLhs permIndices
      -- Add only shapes aInstrEnv, buffers could change during the execution
      existedShape = permAInstrShape ++ mutAInstrShape
      aInstrEnv = updateAInstrEnv PEnd (map snd existedShape)

      -- Compile the index permutation first
      ((permStatement, permExp), (varCount, permFuncMap, permAInstrEnv)) = runState (compileStatement permEnv permBody) (0, OMap.empty, aInstrEnv)

      -- Compile source array buffers
      (glslPermBuffers, permBufferName, buffCount''') = compileAddBufferArgs t' permBuffPrefix buffCount''
      permAInstrBuffs = argBuffers' aIn permBufferName
      permBuffers = map (\(_, (_, x)) -> x) permAInstrBuffs
      -- Compile shource array shapes
      permAInstrShape = argDims' aIn permIndices
      permShape = argDims aIn

      -- Extract the condition and index permutation function
      (cond, permExp') = case permExp of
        TupRpair (TupRsingle (ExpString cond')) (TupRpair TupRunit permExp'') -> (cond', permExp'')
        _ -> error $ "compileOperation: VkPermute: Impossible index permutation function " ++ show permExp
      -- Compile element drop check given the condition
      glslDropCheck = "if (" ++ cond ++ " == 0) { return; } \n"

      -- Compile souce array indexing access for the combination function
      permIdxExp = mapTupR (\(VarName b) -> ExpString $ b ++ "[uint32_t(globalID)]") permBufferName
      ((permVarsStatement, permVars), (varCount', _, _)) =
        runState (newAndBindVars t' "permBuff" permIdxExp) (varCount, OMap.empty, PEnd)

      mutBuffPrefix = "mut"
      -- Compile default mutable array shape
      (glslMutShBuffers, _, mutIndices, mutSh, buffCount') = compileFromIdx sh "mut" buffCount
      -- Compile the default mutable array buffers
      (glslMutBuffers', mutBufferName, buffCount'''') = compileAddBufferArgs t mutBuffPrefix buffCount'''
      mutAInstrBuffs = argBuffers' aMut mutBufferName
      mutBuffers = map (\(_, (_, x)) -> x) mutAInstrBuffs
      mutAInstrShape = argDims' aMut mutIndices
      mutShape = argDims aMut
      -- Compile default array linearized index
      defaultLnIdx@(ExpString lnIdx) = compileToIndex (convertVarName2ExpString mutSh) permExp'
      ((lnIdxStatement, TupRsingle (VarName defaultLnIdx')), (varCount'', _, _)) =
        runState (newAndBindVars tpr "mutIdx" (TupRsingle defaultLnIdx)) (varCount', OMap.empty, PEnd)
        where
          tpr = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt)))
      -- Compile default mutable array indexing access, according to the permutation index
      mutIdxExp = mapTupR (\(VarName b) -> ExpString $ b ++ "[uint32_t(" ++ defaultLnIdx' ++ ")]") mutBufferName
      ((defaultVarsStatement, defaultVars), (varCount''', _, _)) =
        runState (newAndBindVars t "mutBuff" mutIdxExp) (varCount'', OMap.empty, PEnd)

      -- Compile the combination function
      combEnv = updateEnv (updateEnv Empty combLhs1 permVars) combLhs2 defaultVars

      -- Try to compile the combination function using atomic operations
      -- Since no atomic operation supports manipulating multiple buffers at the same time,
      -- the second LHS should only contain a single scalar type.
      -- And because the first LHS has the same type of the second one (https://hackage.haskell.org/package/accelerate-1.3.0.0/docs/Data-Array-Accelerate.html#g:28),
      -- it should also contain a single scalar type.
      --
      (mbCombStatement, (_, combFuncMap', combAInstrEnv'), glslSpecialBuffer) = case fun of
        -- Try to compile exp using atomic exchange
        Lam _ (Lam (LeftHandSideWildcard TupRsingle {}) _) -> (rtn, state, glslMutBuffers')
          where
            (rtn, state) = runState (compileAtomicExchange combEnv mutIdxExp combBody) (varCount'', permFuncMap, permAInstrEnv)

        -- Try to compile atomic operation, treat Float as the bit-cast of Uint32
        Lam lhs1@(LeftHandSideSingle st1) (Lam lhs2@(LeftHandSideSingle st2@(SingleScalarType (NumSingleType (FloatingNumType TypeFloat)))) (Body body)) -> (rtn, state, glslSpecialBuffer')
          where
            ((rtn, isBitsCasted), state) = runState (compileAtomic combExpEnv typeR body) (varCount'', permFuncMap, permAInstrEnv)
            combExpEnv = updateEnv (updateEnv Empty lhs1 permIdxExp) lhs2 mutIdxExp
            typeR = TupRpair (TupRpair TupRunit (TupRsingle st1)) (TupRsingle st2)
            -- If no bit-casting is needed, use the default mutable buffer
            glslSpecialBuffer' = if isBitsCasted then glslSpecialBuffer'' else glslMutBuffers'
            -- Treat Float as the bit-cast of Uint32 for making GLSL buffer binding
            (glslSpecialBuffer'', _, _) = compileAddBufferArgs (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord32)))) mutBuffPrefix buffCount'''
        -- Try to compile atomic operation, treat Double as the bit-cast of Uint64
        Lam lhs1@(LeftHandSideSingle st1) (Lam lhs2@(LeftHandSideSingle st2@(SingleScalarType (NumSingleType (FloatingNumType TypeDouble)))) (Body body)) -> (rtn, state, glslSpecialBuffer')
          where
            ((rtn, isBitsCasted), state) = runState (compileAtomic combExpEnv typeR body) (varCount'', permFuncMap, permAInstrEnv)
            combExpEnv = updateEnv (updateEnv Empty lhs1 permIdxExp) lhs2 mutIdxExp
            typeR = TupRpair (TupRpair TupRunit (TupRsingle st1)) (TupRsingle st2)
            -- If no bit-casting is needed, use the default mutable buffer
            glslSpecialBuffer' = if isBitsCasted then glslSpecialBuffer'' else glslMutBuffers'
            -- Treat Double as the bit-cast of Uint64 for making GLSL buffer binding
            (glslSpecialBuffer'', _, _) = compileAddBufferArgs (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord64)))) mutBuffPrefix buffCount'''

        -- Try to compile exp into lock-free using atomic compare-and-swap
        Lam lhs1@(LeftHandSideSingle st1) (Lam lhs2@(LeftHandSideSingle st2) (Body body)) -> (rtn, state, glslMutBuffers')
          where
            ((rtn, _), state) = runState (compileAtomic combExpEnv typeR body) (varCount'', permFuncMap, permAInstrEnv)
            combExpEnv = updateEnv (updateEnv Empty lhs1 permIdxExp) lhs2 mutIdxExp
            typeR = TupRpair (TupRpair TupRunit (TupRsingle st1)) (TupRsingle st2)
        _ -> (Nothing, (varCount'', permFuncMap, permAInstrEnv), glslMutBuffers')

      -- ((combStatement, combExp), (_, combFuncMap, combAInstrEnv)) = runState (compileStatement combEnv combBody) (varCount'', permFuncMap, permAInstrEnv)

      (combFuncMap, combAInstrEnv, glslSLBuffer, glslSpinLock, lock, glslMutBuffers) = case mbCombStatement of
        Nothing -> (combFuncMap'', combAInstrEnv'', glslSLBuffer', glslSpinLock', lock', glslMutBuffers')
          where
            ((combStatement', combExp'), (_, combFuncMap'', combAInstrEnv'')) = runState (compileStatement combEnv combBody) (varCount'', permFuncMap, permAInstrEnv)
            -- Compile final mutable buffer update
            defaultBufferBnd' = compileUpdateIndexingBuffers mutBufferName (ExpString defaultLnIdx') combExp'
            defaultBufferBnd'' = flattenExpStringTup defaultBufferBnd'
            glslInitSL = unsafePerformIO $ compileGLSL $ glslInitSpinLock buffCount'''' 0
            glslSLBuffer' =
              "layout(set = 0, binding = "
                ++ show buffCount''''
                ++ ", std430) buffer LockBuffer { "
                ++ "int lock[]; "
                ++ "}; \n"
            glslSpinLock' =
              "bool keepWaiting = true; \n"
                ++ "while (keepWaiting) { \n"
                ++ "if (atomicCompSwap(lock[uint32_t("
                ++ lnIdx
                ++ ")], 0, 1) == 0) { \n"
                ++ defaultVarsStatement
                ++ combStatement'
                ++ defaultBufferBnd''
                ++ "memoryBarrier(); \n"
                ++ "keepWaiting = false; \n"
                ++ "atomicExchange(lock[uint32_t("
                ++ lnIdx
                ++ ")], 0); \n"
                ++ "} \n"
                ++ "} \n"
            lock' = Just (glslInitSL, (mutShape, 0))
        Just combStatement' -> (combFuncMap', combAInstrEnv', "", combStatement', Nothing, glslSpecialBuffer)

      -- Delete the existed buffers, to prevent duplicate data trnasfer
      aInstrEnv' = diffPartialEnv combAInstrEnv aInstrEnv
      -- Compile ArrayInstr buffers
      (glslAInstrBuffers, aInstrBuffers, buffCount'') = compileAInstrEnv aInstrEnv' buffCount'
      glslFunc = compileExtraFunc combFuncMap

      -- Buffer inlet order: shape -> ArrayInstr -> In -> Mut -> Out
      glslBuffers = glslPermShBuffers ++ glslMutShBuffers ++ glslAInstrBuffers ++ glslPermBuffers ++ glslMutBuffers ++ glslSLBuffer

      glslBoundaryCheck = compileBoundaryCheck permBufferName "globalID"

      glslMain =
        "void main() { \n"
          ++ glslGlobalID
          ++ glslPermFromIndex
          ++ glslBoundaryCheck
          ++ permStatement
          ++ glslDropCheck
          ++ permVarsStatement
          ++ lnIdxStatement
          ++ glslSpinLock
          ++ "} \n"

      glslCode = glslVersion ++ glslExtension ++ glslGroupSize ++ glslBuffers ++ glslFunc ++ glslMain

      allShapes = permShape ++ mutShape
      inBuffers = aInstrBuffers ++ permBuffers

      spirvCode = unsafePerformIO $ compileGLSL glslCode
      -- Create local Vulkan resources, which is per kernel specific
      vkObjects = unsafePerformIO $ do
        vkLocal <- withLifetime vkGlobalResources (\vkGlobal -> createLocalResources vkGlobal spirvCode lock)
        lt <- newLifetime vkLocal
        addFinalizer lt $ do
          -- say $ pack $ "GC: Destroying local Vulkan resources owned by kernel " ++ show uid
          withLifetime vkGlobalResources (`destroyLocalResources` vkLocal)
        -- say $ pack $ "Creating local Vulkan resources owned by kernel " ++ show uid
        pure lt
      threads = permShape
compileOperation _ op args = error $ "compileOperation: VulkanOp " ++ show op ++ " Args (" ++ show args ++ ") not support"

-- | Compile indeces from GLSL globalID according to the ShapeR, given prefix and started number
--    Gives input buffers for shape and FromIdx expressions, VarNameTup for shape, and VarNameTup for each idx
compileFromIdx :: ShapeR t -> String -> VarCount -> (String, String, VarNameTup t, VarNameTup t, VarCount)
compileFromIdx shr prefix n = (shBuffer, fromIdx, idxTup, shTup, dims)
  where
    ((shBuffer, shTup), dims) = runState (goShape shr) n
    ((fromIdx, idxTup), _) = runState (goIdx shr) (dims - 1, "globalID")

    -- Add dimension args to the GLSL buffer
    goShape :: ShapeR t -> State VarCount (String, VarNameTup t)
    goShape ShapeRz = pure ("", TupRunit)
    goShape (ShapeRsnoc shr) = do
      -- Compile the lower (head) dimensions first
      (shDefL, shNameL) <- goShape shr
      -- Compile the higher dimension
      dimCount <- get
      let shName = prefix ++ "Dim" ++ show dimCount
      let shDef =
            "layout(set = 0, binding = "
              ++ show dimCount
              ++ ", std430) buffer Dim"
              ++ show dimCount
              ++ " { "
              ++ integralTypeToString TypeInt
              ++ " "
              ++ shName
              ++ "; "
              ++ "}; \n"
      put (dimCount + 1)
      return (shDefL ++ shDef, TupRpair shNameL $ TupRsingle $ VarName shName)

    -- Convert elmID (a copy of globalID) to array idx given dimensions
    -- With VarCount and updated globalID as the State
    goIdx :: ShapeR t -> State (VarCount, String) (String, VarNameTup t)
    goIdx ShapeRz = pure ("", TupRunit)
    goIdx (ShapeRsnoc shr) = do
      (idxCount, expr) <- get
      let idxName = prefix ++ "Idx" ++ show idxCount
      let idxDef = "const " ++ integralTypeToString TypeInt ++ " " ++ idxName ++ " = (" ++ expr ++ ") % " ++ prefix ++ "Dim" ++ show idxCount ++ "; \n"
      put (idxCount - 1, "(" ++ expr ++ ") / " ++ prefix ++ "Dim" ++ show idxCount)

      (idxDefL, idxNameL) <- goIdx shr
      return (idxDef ++ idxDefL, TupRpair idxNameL $ TupRsingle $ VarName idxName)

-- | Compile buffer args given TypeR, Prefix and buffer started number, into buffer declarations, and buffer names
compileAddBufferArgs :: TypeR t -> String -> VarCount -> (String, VarNameTup t, VarCount)
compileAddBufferArgs tr prefix n = (bufferDef, bufferName, bufferCount)
  where
    ((bufferDef, bufferName), bufferCount) = runState (go tr) n
    go :: TypeR t' -> State VarCount (String, VarNameTup t')
    go TupRunit = return ("", TupRunit)
    go (TupRsingle (SingleScalarType t)) = do
      bufferCount <- get
      let bufferName = prefix ++ "Buff" ++ show bufferCount
      let bufferDef =
            "layout(set = 0, binding = "
              ++ show bufferCount
              ++ ", std430) buffer Buffer"
              ++ show bufferCount
              ++ " { "
              ++ singleTypeToString t
              ++ " "
              ++ bufferName
              ++ "[]; "
              ++ "}; \n"
      put (bufferCount + 1)
      return (bufferDef, TupRsingle $ VarName bufferName)
    go (TupRpair t1 t2) = do
      (bufferDef, bufferName) <- go t1
      (bufferDef', bufferName') <- go t2
      return (bufferDef ++ bufferDef', TupRpair bufferName bufferName')
    go _ = error "compileAddBufferArgs impossible"

-- | Update buffers returns new buffer expressions, given their names and expressions with their types
compileUpdateBuffers :: VarNameTup t -> ExpStringTup t -> ExpStringTup t
compileUpdateBuffers TupRunit TupRunit = TupRunit
compileUpdateBuffers (TupRsingle (VarName name)) (TupRsingle (ExpString expr)) = TupRsingle (ExpString $ name ++ "[uint32_t(globalID)] = " ++ expr ++ "; \n")
compileUpdateBuffers (TupRpair t1 t2) (TupRpair e1 e2) = TupRpair (compileUpdateBuffers t1 e1) (compileUpdateBuffers t2 e2)
compileUpdateBuffers buffers exps = error $ "updateBuffers: BufferNames (" ++ show buffers ++ ") and Expressions (" ++ show exps ++ ") do not match"

-- | Update buffers returns new buffer expressions, given their names, index, and expressions
compileUpdateIndexingBuffers :: VarNameTup t -> ExpString Int -> ExpStringTup t -> ExpStringTup t
compileUpdateIndexingBuffers TupRunit _ TupRunit = TupRunit
compileUpdateIndexingBuffers (TupRsingle (VarName name)) (ExpString idx) (TupRsingle (ExpString expr)) =
  TupRsingle (ExpString $ name ++ "[uint32_t(" ++ idx ++ ")] = " ++ expr ++ "; \n")
compileUpdateIndexingBuffers (TupRpair t1 t2) idx (TupRpair e1 e2) =
  TupRpair (compileUpdateIndexingBuffers t1 idx e1) (compileUpdateIndexingBuffers t2 idx e2)
compileUpdateIndexingBuffers buffers _ exps = error $ "updateBuffers: BufferNames (" ++ show buffers ++ ") and Expressions (" ++ show exps ++ ") do not match"

-- | Flatten ExpStringTup into String
flattenExpStringTup :: ExpStringTup t -> String
flattenExpStringTup TupRunit = ""
flattenExpStringTup (TupRsingle (ExpString s)) = s
flattenExpStringTup (TupRpair t1 t2) = flattenExpStringTup t1 ++ flattenExpStringTup t2

-- | Check which extra functions are used, add them into the GLSL code
compileExtraFunc :: FuncMap -> String
compileExtraFunc funcMap = funcsStatement
  where
    funcsStatement = concat funcs
    (_, funcs) = unzip (OMap.assocs funcMap)

-- | Compile external called args of array instructions stored in AInstrEnv into GLSL code
compileAInstrEnv :: AInstrEnv benv -> Int -> (String, [Exists (VulkanArg benv)], Int)
compileAInstrEnv env n = (aInstrBuff, vkArg, n')
  where
    args = goEnv weakenId env
    ((aInstrBuff, vkArg), n') = runState (go args) n

    goEnv :: benv' :> benv -> PartialEnv (AInstr benv) benv' -> [(String, Exists (VulkanArg benv))]
    goEnv _ PEnd = []
    goEnv k (PNone env') = goEnv (weakenSucc k) env'
    goEnv k (PPush env' (AInstr name vkArg')) = goEnv (weakenSucc k) env' ++ [(name, Exists vkArg')]

    go :: [(String, Exists (VulkanArg benv))] -> State VarCount (String, [Exists (VulkanArg benv)])
    go [] = return ("", [])
    go ((argName, exist) : xs) = do
      bufferCount <- get
      let bufferDef = case exist of
            Exists (VulkanArg (Var (GroundRbuffer st) _)) ->
              "layout(set = 0, binding = "
                ++ show bufferCount
                ++ ", std430) buffer ArrInstr"
                ++ show bufferCount
                ++ " { "
                ++ scalarTypeToString st
                ++ " "
                ++ argName
                ++ "[]; "
                ++ "}; \n"
            Exists (VulkanArg (Var (GroundRscalar st) _)) ->
              "layout(set = 0, binding = "
                ++ show bufferCount
                ++ ", std430) buffer ArrInstr"
                ++ show bufferCount
                ++ " { "
                ++ scalarTypeToString st
                ++ " "
                ++ argName
                ++ "; "
                ++ "}; \n"
      put (bufferCount + 1)
      (bufferDef', args') <- go xs
      return (bufferDef ++ bufferDef', exist : args')

-- | Update AInstrEnv with VulkanArg
updateAInstrEnv :: AInstrEnv benv -> [(String, Exists (VulkanArg benv))] -> AInstrEnv benv
updateAInstrEnv env [] = env
updateAInstrEnv env [(name, Exists vkArg@(VulkanArg (Var (GroundRbuffer _) idx)))] = partialUpdate (AInstr name vkArg) idx env
updateAInstrEnv env [(name, Exists vkArg@(VulkanArg (Var (GroundRscalar _) idx)))] = partialUpdate (AInstr name vkArg) idx env
updateAInstrEnv env (x : xs) = updateAInstrEnv (updateAInstrEnv env [x]) xs

-- | Make PreOpenExp to get the shape of the array
makePreOpenExpSh :: ShapeR sh -> GroundVars env sh -> PreOpenExp (ArrayInstr env) env' sh
makePreOpenExpSh ShapeRz _ = Nil
makePreOpenExpSh (ShapeRsnoc shr) (TupRpair sh (TupRsingle (Var _ idx))) =
  Pair (makePreOpenExpSh shr sh) (ArrayInstr (Parameter (Var tp idx)) Nil)
  where
    tp = SingleScalarType $ NumSingleType $ IntegralNumType TypeInt
makePreOpenExpSh _ _ = error "makePreOpenExpSh: Impossible"

-- | Compile boundary check for buffers given TupR buffer names and index to be checked
compileBoundaryCheck :: VarNameTup t -> String -> String
compileBoundaryCheck TupRunit _ = ""
compileBoundaryCheck (TupRsingle (VarName buffer)) idx = "if (" ++ idx ++ " >= " ++ buffer ++ ".length()) { return; } \n"
compileBoundaryCheck (TupRpair t1 _) idx = compileBoundaryCheck t1 idx

-- | Make GLSL code for initializing the spin lock
glslInitSpinLock :: Int -> Int32 -> String
glslInitSpinLock binding init' =
  glslVersion
    ++ glslExtension
    ++ glslGroupSize
    ++ "layout(set = 0, binding = "
    ++ show binding
    ++ ", std430) buffer LockBuffer { "
    ++ "int lock[]; "
    ++ "}; \n"
    ++ "void main() { \n"
    ++ glslGlobalID
    ++ "if (globalID < lock.length()) { lock[uint32_t(globalID)] = "
    ++ show init'
    ++ "; } \n"
    ++ "} \n"

glslVersion :: String
glslVersion = "#version 450 \n"

glslExtension :: String
glslExtension =
  "#extension GL_ARB_separate_shader_objects : enable \n"
    -- required for dynamic indexing
    ++ "#extension GL_EXT_nonuniform_qualifier : enable \n"
    -- required for additional arithmetic data types
    ++ "#extension GL_EXT_shader_explicit_arithmetic_types_int8 : enable \n"
    ++ "#extension GL_EXT_shader_explicit_arithmetic_types_int16 : enable \n"
    ++ "#extension GL_EXT_shader_explicit_arithmetic_types_int32 : enable \n"
    ++ "#extension GL_EXT_shader_explicit_arithmetic_types_int64 : enable \n"
    ++ "#extension GL_EXT_shader_explicit_arithmetic_types_float16 : enable \n"
    ++ "#extension GL_EXT_shader_explicit_arithmetic_types_float32 : enable \n"
    ++ "#extension GL_EXT_shader_explicit_arithmetic_types_float64 : enable \n"
    -- required for atomic ops
    ++ "#extension GL_EXT_shader_atomic_int64 : enable \n"
    ++ "#extension GL_EXT_shader_atomic_float : enable \n"

-- | Make local size as small as possible
--    https://computergraphics.stackexchange.com/questions/13866/opengl-compute-shader-how-to-set-local-size
glslGroupSize :: String
glslGroupSize =
  "layout(local_size_x_id = 0) in; \n"
    ++ "layout(local_size_y_id = 1) in; \n"
    ++ "layout(local_size_z_id = 2) in; \n"

-- | Calculation for global ID
--    https://stackoverflow.com/questions/37051316/any-elegant-way-deal-with-array-margins-in-opengl-compute-shaders
glslGlobalID :: String
glslGlobalID = "const " ++ idxTpString ++ " globalID = " ++ idxTpString ++ "(dot(vec3(gl_GlobalInvocationID), vec3(1, gl_NumWorkGroups.x, gl_NumWorkGroups.y * gl_NumWorkGroups.x))); \n"
  where
    idxTpString = integralTypeToString TypeInt
