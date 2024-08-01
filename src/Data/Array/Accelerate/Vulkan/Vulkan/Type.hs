module Data.Array.Accelerate.Vulkan.Vulkan.Type where

import Data.Array.Accelerate.AST.LeftHandSide (Exists)
import Data.Array.Accelerate.Vulkan.Type (VulkanArg)
import Vulkan.Core10
  ( Buffer,
    CommandBuffer,
    CommandPool,
    DescriptorPool,
    DescriptorSet,
    DescriptorSetLayout,
    Device,
    Fence,
    Instance,
    PhysicalDevice,
    Pipeline,
    PipelineLayout,
    Queue,
    ShaderModule,
  )
import Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsMessengerEXT)
import VulkanMemoryAllocator (Allocation, AllocationInfo, Allocator)


data GlobalVulkanResources = GlobalVkResources
  { inst :: Instance,
    debugExt :: DebugUtilsMessengerEXT,
    phys :: PhysicalDevice,
    dev :: Device,
    allocator :: Allocator,
    descriptorPool :: DescriptorPool,
    descriptorSetLayout :: DescriptorSetLayout,
    descriptorSet :: DescriptorSet,
    pipelineLayout :: PipelineLayout,
    cmdPool :: CommandPool,
    cmdBuffer :: CommandBuffer,
    queue :: Queue,
    fence :: Fence
  }

data LocalVulkanResources = LocalVkResources
  { pipeline :: Pipeline,
    shader :: ShaderModule,
    initSpinLock :: Maybe (Pipeline, ShaderModule)
  }
