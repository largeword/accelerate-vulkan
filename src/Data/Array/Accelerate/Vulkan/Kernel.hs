{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Array.Accelerate.Vulkan.Kernel where

import Control.DeepSeq ( NFData(rnf) )
import Data.Array.Accelerate.AST.Kernel (KernelFun, OpenKernelFun(..), NoKernelMetadata)
import Data.Array.Accelerate.AST.Operation
    ( GroundVars,
      Var(Var),
      expType,
      PreOpenFun(Body, Lam),
      Arg(ArgArray, ArgFun),
      Args,
      GroundVar,
      PreArgs((:>:), ArgsNil), AccessGroundR, PreOpenExp (..), ArrayInstr (..), GroundR (..) )
import Data.Array.Accelerate.AST.Partitioned (Clustered (Clustered), Cluster)
import Data.Array.Accelerate.Backend
    ( NFData'(..),
      hashOperation,
      IsKernel(..),
      PrettyKernel(..),
      PrettyKernelStyle(PrettyKernelFun))
import Data.Array.Accelerate.Eval (peekSingletonCluster, applySingletonCluster)
import Data.Array.Accelerate.LLVM.Compile.Cache (UID)
import Data.Array.Accelerate.Representation.Array
    ( Buffer, Buffers, ArrayR(ArrayR) )
import Data.Array.Accelerate.Representation.Shape ( ShapeR(..) )
-- import Data.Array.Accelerate.Vulkan.Compile (compileStatement, singleTypeToString, VarName (VarName), ExpString (ExpString), ExpStringTup, VarCount, FuncMap, VarNameEnv, AInstrMap, scalarTypeToString, AInstrEnv, AInstr (..), VarNameTup, newAndBindVars, integralTypeToString, compileToIndex, convertVarName2ExpString, compileVarsLhs, compileAtomicExchange, compileAtomicPrimFun, compileAtomic)
import Data.Array.Accelerate.Vulkan.Operation ( VulkanOp(..) )
import Data.String ( IsString(fromString) )
import Data.Array.Accelerate.Type (ScalarType(..), SingleType (..), NumType (..), IntegralType (..), FloatingType (..))
import Data.Array.Accelerate.AST.LeftHandSide (LeftHandSide(..), Exists(..))
import Data.Array.Accelerate.Representation.Type (TupR(..), TypeR, reprIsSingle, mapTupR)
import Debug.Trace (trace)
import Data.Array.Accelerate.Pretty.Exp (Adoc)
import Control.Monad.State.Lazy (runState, State, MonadState(put, get))
import Data.Type.Equality ((:~:) (Refl))
import Data.Array.Accelerate.AST.Schedule.Uniform (BaseVars, BaseVar, BaseR (BaseRground))
import Data.Array.Accelerate.Vulkan.Type (VulkanArg (..), VulkanEnv, VulkanElement (..))
import Data.ByteString (ByteString)
import Data.Text (pack)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShader)
import Say (say)
import qualified Data.Map.Ordered as OMap
import Data.Array.Accelerate.AST.Environment (Env (..), PartialEnv (..), partialEnvToList, EnvBinding (..), prj', (:>), weakenSucc, weakenId, partialUpdate, diffPartialEnv)
import Data.Array.Accelerate.AST.Idx (Idx, idxToInt)
import Data.Text.Internal.Lazy.Search (indices)
import Data.Array.Accelerate.Array.Buffer (Buffer(..))
import GHC.Int (Int32)
import Data.Array.Accelerate.Vulkan.Compile.Env
import Data.Array.Accelerate.Vulkan.Compile.Expression
import Data.Array.Accelerate.Vulkan.Compile.Atomic
import Data.Array.Accelerate.Vulkan.Compile.Convert
import Data.Array.Accelerate.Vulkan.Compile.Var
import Data.Array.Accelerate.Vulkan.Compile.LeftHandSide
import Data.Array.Accelerate.Vulkan.Compile.Index
import Data.Array.Accelerate.Representation.Elt (bytesElt)
import GHC.IO.Unsafe (unsafePerformIO)
import Vulkan.Core10 (DescriptorSet, DescriptorSetLayout, PipelineLayout, Pipeline, CommandBuffer, Queue)
import Data.Array.Accelerate.Vulkan.Vulkan.Runtime (compileGLSL, createLocalResources, createGlobalResources, destroyGlobalResources, destroyLocalResources)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Exception.Safe (finally)
import Data.Array.Accelerate.Vulkan.Vulkan.Type (LocalVulkanResources(..), GlobalVulkanResources (..))
import Data.Array.Accelerate.Lifetime (newLifetime, addFinalizer, Lifetime, withLifetime, unsafeGetValue)
import Data.Text (pack)


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
            kernelResources :: Lifetime LocalVulkanResources,
            -- Store scalars to compute the number of threads
            kernelThreads :: [Idx env Int],
            -- Spin locks for permutation.
            -- The fist element of the tuple stores calars to compute the lock length.
            -- The second element of the tuple stores the initial value of each element.
            kernelLock :: Maybe ([Idx env Int], Int32)
          } 
          -> VulkanKernel env

-- | Extract each arg from args of the kernel
argBuffers :: Arg env (m sh e) -> [Exists (VulkanArg env)]
argBuffers (ArgArray _ (ArrayR _ a) _ gvb) = goBuffer a gvb
  where -- | Extract each buffer from buffers
        goBuffer :: forall a env. TypeR a -> GroundVars env (Buffers a) -> [Exists (VulkanArg env)]
        goBuffer TupRunit TupRunit = []
        goBuffer (TupRsingle st) (TupRsingle buff@(Var _ _))
          | Refl <- reprIsSingle @ScalarType @a @Buffer st
          = [Exists (VulkanArg buff)]
        goBuffer (TupRpair t1 t2) (TupRpair gvb1 gvb2)
          = goBuffer t1 gvb1 ++ goBuffer t2 gvb2
        goBuffer _ _ = error "impossible"

-- | Prepare AInstrMap, given Arg and buffer names
argBuffers' :: Arg env (m sh e) -> VarNameTup e -> [(Int, (String, Exists (VulkanArg env)))]
argBuffers' (ArgArray _ (ArrayR _ a) _ gvb) buffNames = go a gvb buffNames
  where go :: forall env' a'. TypeR a' -> GroundVars env' (Buffers a') -> VarNameTup a' -> [(Int, (String, Exists (VulkanArg env')))]
        go TupRunit TupRunit TupRunit = []
        go (TupRsingle st) (TupRsingle buff@(Var _ idx)) (TupRsingle (VarName v))
          | Refl <- reprIsSingle @ScalarType @a' @Buffer st
          = [(idxNum, (v, Exists (VulkanArg buff)))]
          where idxNum = idxToInt idx
        go (TupRpair t1 t2) (TupRpair gvb1 gvb2) (TupRpair v1 v2)
          = go t1 gvb1 v1 ++ go t2 gvb2 v2
        go _ _ _ = error "impossible"



-- -- | Extract each dim from the shape
-- argDims :: forall env a. Arg env a -> [Exists (VulkanArg env)]
-- argDims (ArgArray _ (ArrayR sh _) gv _) = go sh (groundsToBase' gv)
--   where go :: ShapeR sh' -> BaseVars env' sh' -> [Exists (VulkanArg env')]
--         go ShapeRz _  = []
--         go (ShapeRsnoc shr) (TupRpair vars (TupRsingle (Var _ idx))) =
--           go shr vars ++ [Exists (ScalarArg (Var stInt idx))]
--           where stInt = SingleScalarType (NumSingleType (IntegralNumType TypeInt))
--         go _ _ = error "impossible"
-- argDims _ = error "argDims: Only ArgArray is supported"

-- | Extract each dim from the shape
argDims :: Arg env (m sh e) -> [Idx env Int]
argDims (ArgArray _ (ArrayR sh _) gv _) = go sh gv
  where go :: ShapeR sh' -> GroundVars env' sh' -> [Idx env' Int]
        go ShapeRz _  = []
        go (ShapeRsnoc shr) (TupRpair vars (TupRsingle (Var _ idx))) =
          go shr vars ++ [idx]
        go _ _ = error "impossible"
-- argDims _ = error "argDims: Only ArgArray is supported"

-- | Prepare AInstrMap, given shape Arg and shape buffer names
argDims' :: Arg env (m sh e) -> VarNameTup sh -> [(Int, (String, Exists (VulkanArg env)))]
argDims' (ArgArray _ (ArrayR sh _) gv _) buffNames = go sh gv buffNames
  where go :: ShapeR sh' -> GroundVars env' sh' -> VarNameTup sh' -> [(Int, (String, Exists (VulkanArg env')))]
        go ShapeRz TupRunit TupRunit = []
        go (ShapeRsnoc shr) (TupRpair vars (TupRsingle (Var _ idx))) (TupRpair v1 (TupRsingle (VarName v2))) =
          go shr vars v1 ++ [(idxNum, (v2, Exists (VulkanArg (Var (GroundRscalar stInt) idx))))]
          where stInt = SingleScalarType (NumSingleType (IntegralNumType TypeInt))
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
  rnf' (VkKernel !_ sh in' mut out fn thr lc) = unsafeGetValue fn `seq` rnf sh `seq` rnf in' `seq` rnf mut `seq` rnf out `seq` rnf thr `seq` rnf lc

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

  compileKernel :: forall env args. Env AccessGroundR env
    -> Data.Array.Accelerate.AST.Partitioned.Clustered
        (KernelOperation VulkanKernel) args
    -> Args env args -> VulkanKernel env
  compileKernel _ (Clustered clst bclst) clstArgs
    | Just _ <- peekSingletonCluster peekOp clst
    = case (bytesElt (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt)))),
            bytesElt (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord))))) of
        (8, 8) -> applySingletonCluster (compileOperation uid) clst clstArgs
        _ -> error "Accelerate Vulkan backend only supports 64-bit GHC, with 64-bit Int and Word as default"
    where
      uid = hashOperation (Clustered clst bclst) clstArgs

  compileKernel _ _ clstArgs = error $ "compileKernel: Not a singleton cluster with args (" ++ show clstArgs ++ ")"

  -- kernelMetadata :: KernelFun VulkanKernel f -> KernelMetadata VulkanKernel f
  -- kernelMetadata kernel = VulkanKernelMetadata $ sizeOfEnv kernel

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
compileOperation uid VkGenerate args@(ArgFun (Lam lhs (Body body)) :>:
                                  outBuf@(ArgArray m (ArrayR sh t) gv gvb) :>:
                                  ArgsNil)
  -- | bodyTp <- expType body
  --   Just Refl <- isTypeRrefl bodyTp t
  = -- trace (show args) $ 
    -- trace (show t) $ 
    trace glslCode $
    VkKernel uid outShape inBuffers [] outBuffers vkObjects threads Nothing
    where
      idxPrefix = "out"
      -- (glslShBuffers, glslToIndex, outIndices, _, buffCount) = compileToIdx lhs idxPrefix 0
      (glslShBuffers, glslFromIndex, outIndices, _, buffCount) = compileFromIdx sh idxPrefix 0
      newEnv = makeEnv lhs outIndices
      -- ((bodyStatement, bodyExp), (_, funcMap, aInstrMap)) = runState (compileStatement newEnv body) (0, OMap.empty, OMap.empty)
      ((bodyStatement, bodyExp), (_, funcMap, aInstrEnv)) = runState (compileStatement newEnv body) (0, OMap.empty, PEnd)


      bufferPrefix = "out"  -- Set the prefix name of the buffer
      -- (glslAInstrBuffers, inBuffers, buffCount') = compileArrayInstr aInstrMap buffCount
      (glslAInstrBuffers, inBuffers, buffCount') = compileAInstrEnv aInstrEnv buffCount
      -- (glslBuffers, glslBindExp, bufferName) = compileBufferArgs t bodyExp bufferPrefix buffers'
      (glslBuffers, bufferName, _) = compileAddBufferArgs t bufferPrefix buffCount'
      bufferBindExp = compileUpdateBuffers bufferName bodyExp
      glslBindExp = flattenExpStringTup bufferBindExp

      glslFunc = compileExtraFunc funcMap
      -- glslGlobalID = (if idxTpString == "int" then 
      --                   "const int globalID = int(dot(vec3(gl_GlobalInvocationID), vec3(1, gl_NumWorkGroups.x, gl_NumWorkGroups.y * gl_NumWorkGroups.x))); \n" 
      --                 else "const int globalID_ = int(dot(vec3(gl_GlobalInvocationID), vec3(1, gl_NumWorkGroups.x, gl_NumWorkGroups.y * gl_NumWorkGroups.x))); \n" ++
      --                       idxTpString ++ " globalID = " ++ idxTpString ++ "(globalID_); \n")
      -- glslBoundaryCheck = "if (globalID >= " ++ bufferPrefix ++ "0.length()) { return; } \n"
      glslBoundaryCheck = compileBoundaryCheck bufferName "globalID"
      -- glslBindExp = "outbuf[uint32_t(globalID)] = " ++ out ++ "; \n"
      glslMain = "void main() { \n" ++
                  glslGlobalID ++
                  glslFromIndex ++
                  glslBoundaryCheck ++
                  bodyStatement ++
                  glslBindExp ++
                  "} \n"
      glslCode = glslVersion ++ glslExtension ++ glslGroupSize ++ glslShBuffers ++ glslAInstrBuffers ++ glslBuffers ++ glslFunc ++ glslMain
      spirvCode = unsafePerformIO $ compileGLSL glslCode

      -- vkObjects = unsafePerformIO $ createLocalResources vkGlobalResources spirvCode
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

      -- Calculate the number of threads, which will be launched to execute the kernel
      -- threads :: VulkanEnv env -> Int
      -- threads env = case head outBuffers of
      --                 (Exists (BufferArg _ (Var _ idx))) -> case prj' idx env of
      --                   Buffer' _ (Buffer n _) _ -> n
      --                   _ -> error "compileOperation: VkGenerate: Output buffer do not match the buffer type"
      --                 _ -> error "compileOperation: VkGenerate: Cannot find the output buffer"
      threads = outShape


-- !NOTE: combLhs1 and comLhs2 correspond to the source (perm) and default (mut) values respectively
compileOperation uid VkPermute args@(ArgFun fun@(Lam combLhs1 (Lam combLhs2 (Body combBody))) :>:
                                    aMut@(ArgArray m (ArrayR sh t) gv gvb) :>:
                                    ArgFun (Lam permLhs (Body permBody)) :>:
                                    aIn@(ArgArray m' (ArrayR sh' t') gv' gvb') :>:
                                    ArgsNil)
  -- | bodyTp <- expType permBody
  = -- trace (show combLhs1 ++ show combLhs2) $
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
    trace glslCode $
    -- error "not implemented"
    VkKernel uid allShape inBuffers mutBuffers [] vkObjects threads lock
  where
    permIdxPrefix = "perm"
    permBuffPrefix = "perm"
    -- Compile source array shape and index
    (glslPermShBuffers, glslPermFromIndex, permIndices, permSh, buffCount) = compileFromIdx sh' permIdxPrefix 0
    permEnv = makeEnv permLhs permIndices
    -- Add shapes and buffers to the AInstrMap
    -- existedBuffers = permAInstrBuffs ++ mutAInstrBuffs
    existedShape = permAInstrShape ++ mutAInstrShape
    -- aInstrMap = OMap.fromList $ existedShape
    aInstrEnv = updateAInstrEnv PEnd (map snd existedShape)

    -- Compile the index permutation first
    ((permStatement, permExp), (varCount, permFuncMap, permAInstrEnv)) = runState (compileStatement permEnv permBody) (0, OMap.empty, aInstrEnv)

    -- Compile input permutation buffers
    (glslPermBuffers, permBufferName, buffCount''') = compileAddBufferArgs t' permBuffPrefix buffCount''
    permAInstrBuffs = argBuffers' aIn permBufferName
    permBuffers = map (\(_, (_, x)) -> x) permAInstrBuffs
    permAInstrShape = argDims' aIn permIndices
    permShape = argDims aIn
    -- Extract the condition and index permutation function
    (cond, permExp') = case permExp of
      TupRpair (TupRsingle (ExpString cond')) (TupRpair TupRunit permExp'') -> (cond', permExp'')
      _ -> error $ "compileOperation: VkPermute: Impossible index permutation function " ++ show permExp

    -- Compile element drop check given the condition
    glslDropCheck = "if (" ++ cond ++ " == 0) { return; } \n"

    -- Compile souce array indexing access
    permIdxExp = mapTupR (\(VarName b) -> ExpString $ b ++ "[uint32_t(globalID)]") permBufferName
    ((permVarsStatement, permVars), (varCount', _, _))
      = runState (newAndBindVars t' "permBuff" permIdxExp) (varCount, OMap.empty, PEnd)


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
    ((lnIdxStatement, TupRsingle (VarName defaultLnIdx')), (varCount'', _, _))
      = runState (newAndBindVars tpr "mutIdx" (TupRsingle defaultLnIdx)) (varCount', OMap.empty, PEnd)
      where tpr =  TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt)))
    -- Compile default mutable array indexing access, according to the permutation index
    mutIdxExp = mapTupR (\(VarName b) -> ExpString $ b ++ "[uint32_t(" ++ defaultLnIdx' ++ ")]") mutBufferName
    ((defaultVarsStatement, defaultVars), (varCount''', _, _))
      = runState (newAndBindVars t "mutBuff" mutIdxExp) (varCount'', OMap.empty, PEnd)

    -- Compile the combination function
    combEnv = compileVarsLhs (compileVarsLhs Empty combLhs1 permVars) combLhs2 defaultVars

    -- | Try to compile the combination function using atomic operations
    -- Since no atomic operation supports manipulating multiple buffers at the same time,
    -- the second LHS should only contain a single scalar type.
    -- And because the first LHS has the same type of the second one (https://hackage.haskell.org/package/accelerate-1.3.0.0/docs/Data-Array-Accelerate.html#g:28),
    -- it should also contain a single scalar type.
    -- 
    (mbCombStatement, (_, combFuncMap', combAInstrEnv'), glslSpecialBuffer) = case fun of
      -- Try to compile exp using atomic exchange
      Lam _ (Lam (LeftHandSideWildcard TupRsingle{}) _) -> (rtn, state, glslMutBuffers')
        where (rtn, state) = runState (compileAtomicExchange combEnv mutIdxExp combBody) (varCount'', permFuncMap, permAInstrEnv)
      
      -- Try to compile atomic operation, treat Float as the bit-cast of Uint32
      Lam lhs1@(LeftHandSideSingle st1) (Lam lhs2@(LeftHandSideSingle st2@(SingleScalarType (NumSingleType (FloatingNumType TypeFloat)))) (Body body)) -> (rtn, state, glslSpecialBuffer')
        where ((rtn, isBitsCasted), state) = runState (compileAtomic combExpEnv typeR body) (varCount'', permFuncMap, permAInstrEnv)
              combExpEnv = compileVarsLhs (compileVarsLhs Empty lhs1 permIdxExp) lhs2 mutIdxExp
              typeR = TupRpair (TupRpair TupRunit (TupRsingle st1)) (TupRsingle st2)
              -- If no bit-casting is needed, use the default mutable buffer
              glslSpecialBuffer' = if isBitsCasted then glslSpecialBuffer'' else glslMutBuffers'
              -- Treat Float as the bit-cast of Uint32 for making GLSL buffer binding
              (glslSpecialBuffer'', _, _) = compileAddBufferArgs (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord32)))) mutBuffPrefix buffCount'''
      -- Try to compile atomic operation, treat Double as the bit-cast of Uint64
      Lam lhs1@(LeftHandSideSingle st1) (Lam lhs2@(LeftHandSideSingle st2@(SingleScalarType (NumSingleType (FloatingNumType TypeDouble)))) (Body body)) -> (rtn, state, glslSpecialBuffer')
        where ((rtn, isBitsCasted), state) = runState (compileAtomic combExpEnv typeR body) (varCount'', permFuncMap, permAInstrEnv)
              combExpEnv = compileVarsLhs (compileVarsLhs Empty lhs1 permIdxExp) lhs2 mutIdxExp
              typeR = TupRpair (TupRpair TupRunit (TupRsingle st1)) (TupRsingle st2)
              -- If no bit-casting is needed, use the default mutable buffer
              glslSpecialBuffer' = if isBitsCasted then glslSpecialBuffer'' else glslMutBuffers'
              -- Treat Double as the bit-cast of Uint64 for making GLSL buffer binding
              (glslSpecialBuffer'', _, _) = compileAddBufferArgs (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord64)))) mutBuffPrefix buffCount'''

      -- Try to compile exp into lock-free using atomic compare-and-swap
      Lam lhs1@(LeftHandSideSingle st1) (Lam lhs2@(LeftHandSideSingle st2) (Body body)) -> (rtn, state, glslMutBuffers')
        where ((rtn, _), state) = runState (compileAtomic combExpEnv typeR body) (varCount'', permFuncMap, permAInstrEnv)
              combExpEnv = compileVarsLhs (compileVarsLhs Empty lhs1 permIdxExp) lhs2 mutIdxExp
              typeR = TupRpair (TupRpair TupRunit (TupRsingle st1)) (TupRsingle st2)
      _ -> (Nothing, (varCount'', permFuncMap, permAInstrEnv), glslMutBuffers')

    -- ((combStatement, combExp), (_, combFuncMap, combAInstrEnv)) = runState (compileStatement combEnv combBody) (varCount'', permFuncMap, permAInstrEnv)

    (combFuncMap, combAInstrEnv, glslSLBuffer, glslSpinLock, lock, initLock, glslMutBuffers) = case mbCombStatement of
      Nothing -> (combFuncMap'', combAInstrEnv'', glslSLBuffer', glslSpinLock', lock', Just glslInitSL, glslMutBuffers')
        where ((combStatement', combExp'), (_, combFuncMap'', combAInstrEnv'')) = runState (compileStatement combEnv combBody) (varCount'', permFuncMap, permAInstrEnv)
              -- Compile final mutable buffer update
              defaultBufferBnd' = compileUpdateIndexingBuffers mutBufferName (ExpString defaultLnIdx') combExp'
              defaultBufferBnd'' = flattenExpStringTup defaultBufferBnd'
              glslInitSL = unsafePerformIO $ compileGLSL $ glslInitSpinLock buffCount'''' 0
              glslSLBuffer' = "layout(set = 0, binding = " ++ show buffCount'''' ++ ", std430) buffer LockBuffer { \n" ++
                "    int lock[]; \n" ++
                "}; \n"
              glslSpinLock' = "bool keepWaiting = true; \n" ++
                              "while (keepWaiting) { \n" ++
                              "if (atomicCompSwap(lock[uint32_t(" ++ lnIdx ++ ")], 0, 1) == 0) { \n" ++
                              defaultVarsStatement ++
                              combStatement' ++
                              defaultBufferBnd'' ++
                              "memoryBarrier(); \n" ++
                              "keepWaiting = false; \n" ++
                              "atomicExchange(lock[uint32_t(" ++ lnIdx ++ ")], 0); \n" ++
                              "} \n" ++
                              "} \n"
              -- Calculate the spin lock length
              -- lenLock :: VulkanEnv env -> Int
              -- lenLock env = case head mutBuffers of
              --               (Exists (BufferArg _ (Var _ idx))) -> case prj' idx env of
              --                 Buffer' _ (Buffer n _) _ -> n
              --                 _ -> error "compileOperation: VkPermute: Default buffer do not match the buffer type"
              --               _ -> error "compileOperation: VkPermute: Cannot find the output buffer"
              -- lenLock :: VulkanEnv env' -> Env (Idx env') env -> Int
              -- lenLock env' env = case head mutBuffers of
              --               (Exists (BufferArg _ (Var _ idx))) -> case prj' (prj' idx env) env' of
              --                 Buffer' _ (Buffer n _) _ -> n
              --                 _ -> error "compileOperation: VkPermute: Default buffer do not match the buffer type"
              --               _ -> error "compileOperation: VkPermute: Cannot find the output buffer"
              lock' = Just (mutShape, 0)

      Just combStatement' -> (combFuncMap', combAInstrEnv', "", combStatement', Nothing, Nothing, glslSpecialBuffer)


    -- -- Compile final mutable buffer update
    -- defaultBufferBnd = compileUpdateIndexingBuffers mutBufferName (ExpString defaultLnIdx') combExp
    -- defaultBufferBnd' = flattenExpStringTup defaultBufferBnd


    -- aInstrMap' = combAInstrMap OMap.\\ aInstrMap  -- Delete the existed buffers, to prevent duplicate data trnasfer
    aInstrEnv' = diffPartialEnv combAInstrEnv aInstrEnv  -- Delete the existed buffers, to prevent duplicate data trnasfer
    -- (glslAInstrBuffers, aInstrBuffers, buffCount'') = compileArrayInstr aInstrMap' buffCount'
    (glslAInstrBuffers, aInstrBuffers, buffCount'') = compileAInstrEnv aInstrEnv' buffCount'
    glslFunc = compileExtraFunc combFuncMap

    -- Buffer order: shape -> ArrayInstr -> In -> Mut -> Out
    glslBuffers = glslPermShBuffers ++ glslMutShBuffers ++ glslAInstrBuffers ++ glslPermBuffers ++ glslMutBuffers ++ glslSLBuffer

    glslBoundaryCheck = compileBoundaryCheck permBufferName "globalID"

    -- glslSLBuffer = "layout(set = 0, binding = " ++ show buffCount'''' ++ ", std430) buffer LockBuffer { \n" ++
    --                 "    int lock[]; \n" ++
    --                 "}; \n"
    -- glslSpinLock = "bool keepWaiting = true; \n" ++
    --                 "while (keepWaiting) { \n" ++
    --                 "if (atomicCompSwap(lock[uint32_t(" ++ lnIdx ++ ")], 0, 1) == 0) { \n" ++
    --                 defaultVarsStatement ++
    --                 combStatement ++
    --                 defaultBufferBnd ++
    --                 "memoryBarrier(); \n" ++
    --                 "keepWaiting = false; \n" ++
    --                 "atomicExchange(lock[uint32_t(" ++ lnIdx ++ ")], 0); " ++
    --                 "} \n" ++
    --                 "} \n"

    glslMain = "void main() { \n" ++
                glslGlobalID ++
                glslPermFromIndex ++
                glslBoundaryCheck ++
                permStatement ++
                glslDropCheck ++
                permVarsStatement ++
                lnIdxStatement ++
                glslSpinLock ++
                -- defaultVarsStatement ++
                -- combStatement ++
                -- defaultBufferBnd' ++
                  "} \n"

    glslCode = glslVersion ++ glslExtension ++ glslGroupSize ++ glslBuffers ++ glslFunc ++ glslMain

    allShape = permShape ++ mutShape
    inBuffers = aInstrBuffers ++ permBuffers

    spirvCode = unsafePerformIO $ compileGLSL glslCode
    -- vkObjects = unsafePerformIO $ createLocalResources vkGlobalResources spirvCode
    -- Create local Vulkan resources, which is per kernel specific
    vkObjects = unsafePerformIO $ do
                vkLocal <- withLifetime vkGlobalResources (\vkGlobal -> createLocalResources vkGlobal spirvCode initLock)
                lt <- newLifetime vkLocal
                addFinalizer lt $ do
                  -- say $ pack $ "GC: Destroying local Vulkan resources owned by kernel " ++ show uid
                  withLifetime vkGlobalResources (`destroyLocalResources` vkLocal)
                -- say $ pack $ "Creating local Vulkan resources owned by kernel " ++ show uid
                pure lt

    -- Calculate the number of threads, which will be launched to execute the kernel
    -- threads :: VulkanEnv env' -> Env (Idx env') env -> Int
    -- threads env' env = case head permBuffers of
    --                 (Exists (BufferArg _ (Var _ idx))) -> case prj' (prj' idx env) env' of
    --                   Buffer' _ (Buffer n _) _ -> n
    --                   _ -> error "compileOperation: VkPermute: Source buffer do not match the buffer type"
    --                 _ -> error "compileOperation: VkPermute: Cannot find the output buffer"
    threads = permShape

    -- -- Calculate the spin lock length
    -- lenLock :: VulkanEnv env -> Int
    -- lenLock env = case head mutBuffers of
    --               (Exists (ArrayArg _ (Var _ idx))) -> case prj' idx env of
    --                 Buffer' _ (Buffer n _) -> n
    --                 _ -> error "compileOperation: VkPermute: Default buffer do not match the buffer type"
    --               _ -> error "compileOperation: VkPermute: Cannot find the output buffer"
    -- lock = (lenLock, 0)







compileOperation _ op args = error $ "compileOperation: VulkanOp " ++ show op ++ " Args (" ++ show args ++ ") not support"


-- -- | Compile Env containing input indeces for Generate
-- compileIdx :: LeftHandSide ScalarType t () env -> String -> VarNameEnv env
-- compileIdx lhs prefix = rtnEnv
--   where (rtnEnv, varCount') = runState (go Empty lhs) 0
--         go :: VarNameEnv env -> LeftHandSide s t env env' -> State VarCount (VarNameEnv env')
--         go env (LeftHandSideWildcard _) = return env
--         go env (LeftHandSideSingle _) = do
--           varCount <- get
--           put (varCount+1)
--           return $ Push env (VarName $ prefix ++ "Idx" ++ show varCount)
--         go env (LeftHandSidePair lhs1 lhs2) = do
--           env' <- go env lhs1
--           go env' lhs2

-- | Make Env containing input indeces for Generate
makeEnv :: LeftHandSide ScalarType t () env -> VarNameTup t -> VarNameEnv env
makeEnv lhs idx = go Empty lhs idx
  where go :: VarNameEnv env -> LeftHandSide s t env env' -> VarNameTup t -> VarNameEnv env'
        go env (LeftHandSideWildcard _) _ = env
        go env (LeftHandSideSingle _) (TupRsingle varName) = Push env varName
        go env (LeftHandSidePair lhs1 lhs2) (TupRpair v1 v2) = go (go env lhs1 v1) lhs2 v2
        go _ _ _ = error $ "makeEnv: LeftHandSide (" ++ show lhs ++ ") and VarNames (" ++ show idx ++ ") do not match"

-- -- | Compile indeces from GLSL globalID according to the LHS, given prefix and started number
-- --    Gives input buffers for shape and ToIdx expressions, VarNameTup for shape, and VarNameTup for each idx
-- compileToIdx :: LeftHandSide ScalarType t () env -> String -> VarCount -> (String, String, VarNameTup t, VarNameTup t, VarCount)
-- compileToIdx lhs prefix n = (shBuffer, toIdx, idxTup, shTup, dims)
--   where ((shBuffer, shTup), dims) = runState (goShape lhs) n
--         ((toIdx, idxTup), _) = runState (goIdx lhs) (dims-1, "globalID")
--         -- Add dimension args to the GLSL buffer
--         goShape :: LeftHandSide ScalarType t env env' -> State VarCount (String, VarNameTup t)
--         goShape (LeftHandSideWildcard TupRunit) = pure ("", TupRunit)
--         goShape (LeftHandSideSingle (SingleScalarType idxTp)) = do
--           dimCount <- get
--           let shName = prefix ++ "Dim" ++ show dimCount
--           let shDef = "layout(set = 0, binding = " ++ show dimCount ++ ", std430) buffer Dim" ++ show dimCount ++ " { \n" ++
--                       "    " ++ singleTypeToString idxTp ++ " " ++ shName ++ "; \n" ++
--                       "}; \n"
--           put (dimCount+1)
--           return (shDef, TupRsingle $ VarName shName)
--         goShape (LeftHandSidePair lhs1 lhs2) = do
--           (s1, v1) <- goShape lhs1  -- Compile the lower (head) dimensions first
--           (s2, v2) <- goShape lhs2
--           return (s1 ++ s2, TupRpair v1 v2)
--         goShape _ = error "compileToIdx impossible"

--         -- Convert elmID (a copy of globalID) to array idx given dimensions
--         -- With VarCount and updated globalID as the State
--         goIdx :: LeftHandSide ScalarType t env env' -> State (VarCount, String) (String, VarNameTup t)
--         goIdx (LeftHandSideWildcard TupRunit) = pure ("", TupRunit)
--         goIdx (LeftHandSideSingle (SingleScalarType idxTp)) = do
--           (idxCount, expr) <- get
--           let idxName = prefix ++ "Idx" ++ show idxCount
--           let idxDef = "const " ++ singleTypeToString idxTp ++ " " ++ idxName ++ " = (" ++ expr ++ ") % " ++ prefix ++ "Dim" ++ show idxCount ++ "; \n"
--           put (idxCount-1, "(" ++ expr ++ ") / " ++ prefix ++ "Dim" ++ show idxCount)
--           -- let newGID = "elmID = elmID / dim" ++ show idxCount ++ "; \n"
--           return (idxDef, TupRsingle $ VarName idxName)
--         goIdx (LeftHandSidePair lhs2 lhs1) = do
--           (idx1, v1) <- goIdx lhs1  -- Compile the higher dimension first
--           (idx2, v2) <- goIdx lhs2
--           return (idx1 ++ idx2, TupRpair v2 v1)
--         goIdx _ = error "compileToIdx impossible"

-- | Compile indeces from GLSL globalID according to the ShapeR, given prefix and started number
--    Gives input buffers for shape and FromIdx expressions, VarNameTup for shape, and VarNameTup for each idx
compileFromIdx :: ShapeR t -> String -> VarCount -> (String, String, VarNameTup t, VarNameTup t, VarCount)
compileFromIdx shr prefix n = (shBuffer, fromIdx, idxTup, shTup, dims)
  where ((shBuffer, shTup), dims) = runState (goShape shr) n
        ((fromIdx, idxTup), _) = runState (goIdx shr) (dims-1, "globalID")

        -- Add dimension args to the GLSL buffer
        goShape :: ShapeR t -> State VarCount (String, VarNameTup t)
        goShape ShapeRz = pure ("", TupRunit)
        goShape (ShapeRsnoc shr) = do
          -- Compile the lower (head) dimensions first
          (shDefL, shNameL) <- goShape shr
          -- Compile the higher dimension
          dimCount <- get
          let shName = prefix ++ "Dim" ++ show dimCount
          let shDef = "layout(set = 0, binding = " ++ show dimCount ++ ", std430) buffer Dim" ++ show dimCount ++ " { \n" ++
                      "    " ++ integralTypeToString TypeInt ++ " " ++ shName ++ "; \n" ++
                      "}; \n"
          put (dimCount+1)
          return (shDefL ++ shDef, TupRpair shNameL $ TupRsingle $ VarName shName)

        -- Convert elmID (a copy of globalID) to array idx given dimensions
        -- With VarCount and updated globalID as the State
        goIdx ::  ShapeR t -> State (VarCount, String) (String, VarNameTup t)
        goIdx ShapeRz = pure ("", TupRunit)
        goIdx (ShapeRsnoc shr)  = do
          (idxCount, expr) <- get
          let idxName = prefix ++ "Idx" ++ show idxCount
          let idxDef = "const " ++ integralTypeToString TypeInt ++ " " ++ idxName ++ " = (" ++ expr ++ ") % " ++ prefix ++ "Dim" ++ show idxCount ++ "; \n"
          put (idxCount-1, "(" ++ expr ++ ") / " ++ prefix ++ "Dim" ++ show idxCount)

          (idxDefL, idxNameL) <- goIdx shr
          return (idxDef ++ idxDefL, TupRpair idxNameL $ TupRsingle $ VarName idxName)

-- compileBufferIndexing :: 

-- -- | Compile ExpStringTup given TypeR and Prefix into buffer declarations, buffer bindings, and buffer started number
-- compileAddBufferArgs :: TypeR t -> ExpStringTup t -> String -> VarCount -> (String, String, VarNameTup t)
-- compileAddBufferArgs tr expTup prefix n = buffers
--   where (buffers, _) = runState (go tr expTup) n
--         go :: TypeR t' -> ExpStringTup t' -> State VarCount (String, String, VarNameTup t')
--         go TupRunit TupRunit = return ("", "", TupRunit)
--         go (TupRsingle (SingleScalarType t)) (TupRsingle (ExpString expr)) = do
--           bufferCount <- get
--           let bufferName = prefix ++ show bufferCount
--           let bufferDef = "layout(set = 0, binding = " ++ show bufferCount ++ ", std430) buffer Buffer" ++ show bufferCount ++ " { \n" ++
--                             "    " ++ singleTypeToString t ++ " " ++ bufferName ++ "[]; \n" ++
--                             "}; \n"
--           let bufferBnd = bufferName ++ "[uint32_t(globalID)] = " ++ expr ++ "; \n"
--           put (bufferCount + 1)
--           return (bufferDef, bufferBnd, TupRsingle $ VarName bufferName)
--         go (TupRpair t1 t2) (TupRpair e1 e2) = do
--           (bufferDef, bufferBnd, bufferName) <- go t1 e1
--           (bufferDef', bufferBnd', bufferName') <- go t2 e2
--           return (bufferDef ++ bufferDef', bufferBnd ++ bufferBnd', TupRpair bufferName bufferName')
--         go _ _ = error "compileBufferArgs impossible"

-- | Compile buffer args given TypeR, Prefix and buffer started number, into buffer declarations, and buffer names
compileAddBufferArgs :: TypeR t -> String -> VarCount -> (String, VarNameTup t, VarCount)
compileAddBufferArgs tr prefix n = (bufferDef, bufferName, bufferCount)
  where ((bufferDef, bufferName), bufferCount) = runState (go tr) n
        go :: TypeR t' -> State VarCount (String, VarNameTup t')
        go TupRunit = return ("", TupRunit)
        go (TupRsingle (SingleScalarType t)) = do
          bufferCount <- get
          let bufferName = prefix ++ "Buff" ++ show bufferCount
          let bufferDef = "layout(set = 0, binding = " ++ show bufferCount ++ ", std430) buffer Buffer" ++ show bufferCount ++ " { \n" ++
                            "    " ++ singleTypeToString t ++ " " ++ bufferName ++ "[]; \n" ++
                            "}; \n"
          put (bufferCount + 1)
          return (bufferDef, TupRsingle $ VarName bufferName)
        go (TupRpair t1 t2) = do
          (bufferDef, bufferName) <- go t1
          (bufferDef', bufferName') <- go t2
          return (bufferDef ++ bufferDef', TupRpair bufferName bufferName')
        go _ = error "compileAddBufferArgs impossible"

-- | Update buffers returns new buffer expressions, given their names and expressions with their types
compileUpdateBuffers :: VarNameTup t -> ExpStringTup t-> ExpStringTup t
compileUpdateBuffers TupRunit TupRunit = TupRunit
-- compileUpdateBuffers (TupRsingle (VarName name)) (TupRsingle (ExpString expr)) (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord8)))) = TupRsingle (ExpString $ name ++ "[uint32_t(globalID)] = uint8_t(" ++ expr ++ "); \n")
compileUpdateBuffers (TupRsingle (VarName name)) (TupRsingle (ExpString expr)) = TupRsingle (ExpString $ name ++ "[uint32_t(globalID)] = " ++ expr ++ "; \n")
-- compileUpdateBuffers (TupRsingle (VarName _)) (TupRsingle (ExpString _)) (TupRsingle (VectorScalarType _)) = error "compileUpdateBuffers: Vector type not supported"
compileUpdateBuffers (TupRpair t1 t2) (TupRpair e1 e2) = TupRpair (compileUpdateBuffers t1 e1) (compileUpdateBuffers t2 e2)
compileUpdateBuffers buffers exps = error $ "updateBuffers: BufferNames (" ++ show buffers ++ ") and Expressions (" ++ show exps ++ ") do not match"

-- | Update buffers returns new buffer expressions, given their names, index, and expressions
compileUpdateIndexingBuffers :: VarNameTup t -> ExpString Int -> ExpStringTup t -> ExpStringTup t
compileUpdateIndexingBuffers TupRunit _ TupRunit = TupRunit
compileUpdateIndexingBuffers (TupRsingle (VarName name)) (ExpString idx) (TupRsingle (ExpString expr))
  = TupRsingle (ExpString $ name ++ "[uint32_t(" ++ idx ++ ")] = " ++ expr ++ "; \n")
compileUpdateIndexingBuffers (TupRpair t1 t2) idx (TupRpair e1 e2)
  = TupRpair (compileUpdateIndexingBuffers t1 idx e1) (compileUpdateIndexingBuffers t2 idx e2)
compileUpdateIndexingBuffers buffers _ exps = error $ "updateBuffers: BufferNames (" ++ show buffers ++ ") and Expressions (" ++ show exps ++ ") do not match"

-- | Flatten ExpStringTup into String
flattenExpStringTup :: ExpStringTup t -> String
flattenExpStringTup TupRunit = ""
flattenExpStringTup (TupRsingle (ExpString s)) = s
flattenExpStringTup (TupRpair t1 t2) = flattenExpStringTup t1 ++ flattenExpStringTup t2

-- | Check which extra functions are used, add them into the GLSL code
compileExtraFunc :: FuncMap -> String
compileExtraFunc funcMap = funcsStatement
  where funcsStatement = concat funcs
        (_, funcs) = unzip (OMap.assocs funcMap)

-- -- | Compile external called args of array instructions into GLSL code
-- compileArrayInstr :: AInstrMap benv -> Int -> (String, [Exists (VulkanArg benv)], Int)
-- compileArrayInstr aInstrMap n = (buffers, vkArgs, args)
--   where (_, vkArgs) = unzip arrayInstr
--         (buffers, args) = runState (go arrayInstr) n
--         (_, arrayInstr) = unzip (OMap.assocs aInstrMap)
--         go :: [(String, Exists (VulkanArg benv))] -> State VarCount String
--         go [] = return ""
--         go ((argName, exist):xs) = do
--           bufferCount <- get
--           let bufferDef = case exist of
--                           Exists (VulkanArg (Var (GroundRbuffer st) _)) ->
--                             "layout(set = 0, binding = " ++ show bufferCount ++ ", std430) buffer ArrInstr" ++ show bufferCount ++ " { \n" ++
--                             "    " ++ scalarTypeToString st ++ " " ++ argName ++ "[]; \n" ++
--                             "}; \n"
--                           Exists (VulkanArg (Var (GroundRscalar st) _)) ->
--                             "layout(set = 0, binding = " ++ show bufferCount ++ ", std430) buffer ArrInstr" ++ show bufferCount ++ " { \n" ++
--                             "    " ++ scalarTypeToString st ++ " " ++ argName ++ "; \n" ++
--                             "}; \n"
--                           _ -> error "impossible"
--           put (bufferCount + 1)
--           bufferDef' <- go xs
--           return $ bufferDef ++ bufferDef'
--         -- go _ = error "compileBufferArgs impossible"

-- | Compile external called args of array instructions stored in AInstrEnv into GLSL code
compileAInstrEnv :: AInstrEnv benv -> Int -> (String, [Exists (VulkanArg benv)], Int)
compileAInstrEnv env n = (aInstrBuff, vkArg, n')
  where
        -- args = partialEnvToList env
        args = goEnv weakenId env
        ((aInstrBuff, vkArg), n') = runState (go args) n
        -- args' = go args
        -- (argsName, vkArgs) = unzip args'
        -- (buffers, argsCount) = trace (show argsName) $ runState (go' args') n

        goEnv :: benv' :> benv -> PartialEnv (AInstr benv) benv' -> [(String, Exists (VulkanArg benv))]
        goEnv _ PEnd = []
        goEnv k (PNone env') = goEnv (weakenSucc k) env'
        goEnv k (PPush env' (AInstr name vkArg')) = goEnv (weakenSucc k) env' ++ [(name, Exists vkArg')]

        go :: [(String, Exists (VulkanArg benv))] -> State VarCount (String, [Exists (VulkanArg benv)])
        go [] = return ("", [])
        go ((argName, exist):xs) = do
          bufferCount <- get
          let bufferDef = case exist of
                          Exists (VulkanArg (Var (GroundRbuffer st) _)) ->
                            "layout(set = 0, binding = " ++ show bufferCount ++ ", std430) buffer ArrInstr" ++ show bufferCount ++ " { \n" ++
                            "    " ++ scalarTypeToString st ++ " " ++ argName ++ "[]; \n" ++
                            "}; \n"
                          Exists (VulkanArg (Var (GroundRscalar st) _)) ->
                            "layout(set = 0, binding = " ++ show bufferCount ++ ", std430) buffer ArrInstr" ++ show bufferCount ++ " { \n" ++
                            "    " ++ scalarTypeToString st ++ " " ++ argName ++ "; \n" ++
                            "}; \n"
          put (bufferCount + 1)
          (bufferDef', args') <- go xs
          return (bufferDef ++ bufferDef', exist:args')

updateAInstrEnv :: AInstrEnv benv -> [(String, Exists (VulkanArg benv))] -> AInstrEnv benv
updateAInstrEnv env [] = env
updateAInstrEnv env [(name, Exists vkArg@(VulkanArg (Var (GroundRbuffer _) idx)))] = partialUpdate (AInstr name vkArg) idx env
updateAInstrEnv env [(name, Exists vkArg@(VulkanArg (Var (GroundRscalar _) idx)))] = partialUpdate (AInstr name vkArg) idx env
updateAInstrEnv env (x:xs) = updateAInstrEnv (updateAInstrEnv env [x]) xs

-- -- | Convert index into the array into linearized index, given shape and index
-- toIndexExp :: ShapeR sh -> GroundVars env sh -> PreOpenExp (ArrayInstr env) env' sh -> PreOpenExp (ArrayInstr env) env' Int
-- toIndexExp ShapeRz _ _ = Const (SingleScalarType $ NumSingleType $ IntegralNumType TypeInt) 0
-- toIndexExp (ShapeRsnoc shr) (TupRpair sh (TupRsingle (Var groudR idx))) (Pair exp1 exp2) = error "toIndexExp: impossible"

-- | Make PreOpenExp to get the shape of the array
makePreOpenExpSh :: ShapeR sh -> GroundVars env sh -> PreOpenExp (ArrayInstr env) env' sh
makePreOpenExpSh ShapeRz _ = Nil
makePreOpenExpSh (ShapeRsnoc shr) (TupRpair sh (TupRsingle (Var _ idx)))
  = Pair (makePreOpenExpSh shr sh) (ArrayInstr (Parameter (Var tp idx)) Nil)
    where tp = SingleScalarType $ NumSingleType $ IntegralNumType TypeInt
makePreOpenExpSh _ _ = error "makePreOpenExpSh: Impossible"


-- | Compile boundary check for buffers given TupR buffer names and index to be checked
-- !TODO: only check one buffer, since they have the same length
compileBoundaryCheck :: VarNameTup t -> String -> String
compileBoundaryCheck TupRunit _ = ""
compileBoundaryCheck (TupRsingle (VarName buffer)) idx = "if (" ++ idx ++ " >= " ++ buffer ++ ".length()) { return; } \n"
-- compileBoundaryCheck (TupRpair t1 t2) idx = compileBoundaryCheck t1 idx ++ compileBoundaryCheck t2 idx
compileBoundaryCheck (TupRpair t1 _) idx = compileBoundaryCheck t1 idx

-- | Make GLSL code for initializing the spin lock
glslInitSpinLock :: Int -> Int32 -> String
glslInitSpinLock binding init' = glslVersion ++
                                  glslExtension ++
                                  glslGroupSize ++
                                  "layout(set = 0, binding = " ++ show binding ++ ", std430) buffer LockBuffer { \n" ++
                                  "    int lock[]; \n" ++
                                  "}; \n" ++
                                  "void main() { \n" ++
                                  glslGlobalID ++
                                  "if (globalID < lock.length()) { lock[uint32_t(globalID)] = " ++ show init' ++ "; } \n" ++
                                  "} \n"

-- -- | Create shader module to initialize the spin lock
-- vkInitializeSpinLock :: IO ()
-- vkInitializeSpinLock = unsafePerformIO $ do
--                         spirvCode <- compileGLSL $ glslSpinLock 0
--                         vkLocal <- withLifetime vkGlobalResources (`createLocalResources` spirvCode)
--                         lt <- newLifetime vkLocal
--                         addFinalizer lt $ do
--                           -- say $ pack $ "GC: Destroying local Vulkan resources owned by kernel " ++ show uid
--                           withLifetime vkGlobalResources (`destroyLocalResources` vkLocal)
--                         -- say $ pack $ "Creating local Vulkan resources owned by kernel " ++ show uid
--                         pure lt

glslVersion :: String
glslVersion = "#version 450 \n"

glslExtension :: String
glslExtension = "#extension GL_ARB_separate_shader_objects : enable \n"
                -- ++ "#extension GL_KHR_vulkan_glsl : enable \n"
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
-- glslGroupSize = "layout(local_size_x = 256, local_size_y = 1, local_size_z = 1) in; \n"
glslGroupSize = "layout(local_size_x_id = 0) in; \n" ++
                "layout(local_size_y_id = 1) in; \n" ++
                "layout(local_size_z_id = 2) in; \n"
                -- "layout(local_size_x_id = 0, local_size_y_id = 1, local_size_z_id = 2) in; \n"

-- | Calculation for global ID
--    https://stackoverflow.com/questions/37051316/any-elegant-way-deal-with-array-margins-in-opengl-compute-shaders
glslGlobalID :: String
glslGlobalID = "const " ++ idxTpString ++ " globalID = " ++ idxTpString ++ "(dot(vec3(gl_GlobalInvocationID), vec3(1, gl_NumWorkGroups.x, gl_NumWorkGroups.y * gl_NumWorkGroups.x))); \n"
                where idxTpString = singleTypeToString (NumSingleType (IntegralNumType TypeInt))






