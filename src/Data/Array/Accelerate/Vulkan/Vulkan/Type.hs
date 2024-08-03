module Data.Array.Accelerate.Vulkan.Vulkan.Type where

import Data.Array.Accelerate.AST.Idx (Idx)
import Data.Int (Int32)
import Vulkan.Core10
  ( CommandBuffer,
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
import VulkanMemoryAllocator (Allocator)

data GlobalVulkanResources = GlobalVkResources
  { inst :: Instance,
    debugExt :: Maybe DebugUtilsMessengerEXT,
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

data LocalVulkanResources env = LocalVkResources
  { pipeline :: Pipeline,
    shader :: ShaderModule,
    initSpinLock ::
      Maybe
        ( Pipeline,
          ShaderModule,
          -- Spin locks for permutation.
          -- The fist element of the tuple stores scalars of dimensional numbers to compute the lock length.
          -- The second element of the tuple stores the initial value of each element.
          ([Idx env Int], Int32)
        )
  }
