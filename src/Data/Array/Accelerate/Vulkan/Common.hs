{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Array.Accelerate.Vulkan.Common where

import Control.Concurrent (MVar)
import Control.DeepSeq (NFData (..))
import Data.Array.Accelerate.AST.Environment (Env (..))
import Data.Array.Accelerate.AST.LeftHandSide (Exists (..), LeftHandSide (..))
import Data.Array.Accelerate.AST.Operation (GroundR (GroundRbuffer, GroundRscalar), GroundVar, Var (..))
import Data.Array.Accelerate.Array.Buffer (Buffer)
import Data.Array.Accelerate.Lifetime (Lifetime)
import Data.Array.Accelerate.Representation.Type (TupR (..))
import Data.Array.Accelerate.Type
  ( ScalarType (..),
  )
import Foreign.Ptr (Ptr)
import qualified Vulkan.Core10 as Vk (Buffer)
import VulkanMemoryAllocator (Allocation, AllocationInfo)

newtype VulkanArg env a = VulkanArg (GroundVar env a)

instance Show (VulkanArg env a) where
  show (VulkanArg (Var (GroundRbuffer st) _)) = "ArrayArg " ++ show st
  show (VulkanArg (Var (GroundRscalar st) _)) = "ScalarArg" ++ show st

instance Show (Exists (VulkanArg env)) where
  show (Exists arg) = "Exists " ++ show arg

instance NFData (Exists (VulkanArg env)) where
  rnf (Exists _) = ()

data VulkanElement a where
  Scalar' :: ScalarType a -> a -> Lifetime ((Vk.Buffer, Allocation, AllocationInfo), Ptr ()) -> VulkanElement a
  Buffer' :: ScalarType a -> (Buffer a) -> Lifetime ((Vk.Buffer, Allocation, AllocationInfo), Ptr ()) -> VulkanElement (Buffer a)

type VulkanElements = TupR VulkanElement

type VulkanEnv = Env VulkanElement

-- | Convert IOFun to concrete types
type family VulkanElementsIOFun t where
  VulkanElementsIOFun (MVar a -> ()) = IO a
  VulkanElementsIOFun (a -> b) = VulkanElements a -> VulkanElementsIOFun b
  VulkanElementsIOFun t = IO t

-- | Update Env with LeftHandSide and TupR
updateEnv :: Env f env -> LeftHandSide s bnd_t env env' -> TupR f bnd_t -> Env f env'
updateEnv env (LeftHandSideWildcard _) _ = env
updateEnv env (LeftHandSideSingle _) (TupRsingle v) = Push env v
updateEnv env (LeftHandSidePair lhs1 lhs2) (TupRpair v1 v2) = updateEnv (updateEnv env lhs1 v1) lhs2 v2
updateEnv _ _ _ = error "updateEnv: LeftHandSide and TupR do not match"
