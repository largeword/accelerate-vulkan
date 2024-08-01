{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Array.Accelerate.Vulkan.Type where

import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.Word ( Word8, Word16, Word32, Word64 )
import qualified Data.Vector.Storable as S
import Data.Complex ( Complex )
import Data.Array.Accelerate.Type
    ( FloatingType(..),
      IntegralType(..),
      NumType(..),
      SingleType(..), ScalarType(..), IntegralDict(..), Half )
import Data.Array.Accelerate.Representation.Type (TypeR, TupR (..), Distributes (..), mapTupR)
import Data.Array.Accelerate.Array.Buffer (Buffer, Buffers)
import Data.Array.Accelerate.AST.Environment (Env, prj')
import Control.Concurrent (MVar)
import Data.Array.Accelerate.Representation.Shape (ShapeR (..))
import Data.Array.Accelerate.AST.Operation (Vars, Var (..), GroundVars, GroundVar, ExpVar, NFData' (..), GroundR (GroundRbuffer, GroundRscalar), ExpVars)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import Foreign (peekArray, pokeArray)
import Data.Type.Equality ((:~:) (..))
import Data.Primitive.Vec (Vec)
import Data.Array.Accelerate.AST.Schedule.Uniform (BaseVars, BaseVar, BaseR (..))
import Data.Array.Accelerate.AST.LeftHandSide (Exists (..))


import qualified Vulkan.Core10 as Vk (DescriptorSet, DescriptorSetLayout, PipelineLayout, Pipeline, CommandBuffer, Queue, Instance, PhysicalDevice, Device, DescriptorPool, ShaderModule, CommandPool, Buffer)
import VulkanMemoryAllocator (Allocator, Allocation, AllocationInfo)
import Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsMessengerEXT)
import Data.ByteString (ByteString)
import Control.DeepSeq (NFData (..))
import Data.Array.Accelerate.Lifetime (Lifetime)


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
  VulkanElementsIOFun (a -> b)       = VulkanElements a -> VulkanElementsIOFun b
  VulkanElementsIOFun t              = IO t
