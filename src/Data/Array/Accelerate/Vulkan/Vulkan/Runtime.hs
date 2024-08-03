{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Array.Accelerate.Vulkan.Vulkan.Runtime where

import Control.Exception.Safe
  ( MonadThrow,
    catch,
    throwString,
  )
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Array.Accelerate.AST.Environment (Env, prj')
import Data.Array.Accelerate.AST.Idx (Idx)
import Data.Array.Accelerate.AST.LeftHandSide (Exists (..))
import Data.Array.Accelerate.AST.Operation (GroundR (GroundRbuffer, GroundRscalar))
import Data.Array.Accelerate.AST.Var (Var (..))
import Data.Array.Accelerate.Array.Buffer (ScalarArrayDataR, SingleArrayDict (..), singleArrayDict)
import qualified Data.Array.Accelerate.Array.Buffer as AAB
import Data.Array.Accelerate.Array.Unique (withUniqueArrayPtr)
import Data.Array.Accelerate.Lifetime (Lifetime, touchLifetime, unsafeGetValue, withLifetime)
import Data.Array.Accelerate.Representation.Elt (bytesElt)
import Data.Array.Accelerate.Representation.Type (TupR (..))
import Data.Array.Accelerate.Type (ScalarType (..), SingleDict (..), SingleType (..), singleDict)
import Data.Array.Accelerate.Vulkan.Common (VulkanArg (..), VulkanElement (..), VulkanEnv)
import Data.Array.Accelerate.Vulkan.Vulkan.Type (GlobalVulkanResources)
import qualified Data.Array.Accelerate.Vulkan.Vulkan.Type as R (GlobalVulkanResources (..), LocalVulkanResources (..))
import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import Data.Foldable (Foldable (toList), for_)
import Data.Int (Int32)
import Data.List (partition)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import Foreign.Marshal (copyArray)
import Foreign.Marshal.Array (newArray)
import Foreign.Ptr (Ptr, castFunPtr, castPtr)
import Foreign.Storable (Storable (peek, poke), sizeOf)
import GHC.IO (unsafePerformIO)
import Say (say, sayErr)
import System.Mem (performGC)
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import Vulkan.Core10
  ( ApplicationInfo (apiVersion, applicationName),
    Buffer,
    BufferCopy (..),
    BufferCreateInfo (size, usage),
    BufferUsageFlagBits (..),
    BufferUsageFlags,
    CommandBuffer (commandBufferHandle),
    CommandBufferAllocateInfo (commandBufferCount, commandPool, level),
    CommandBufferBeginInfo (flags),
    CommandBufferLevel (COMMAND_BUFFER_LEVEL_PRIMARY),
    CommandBufferResetFlagBits (COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT),
    CommandBufferUsageFlagBits (COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT),
    CommandPool,
    CommandPoolCreateFlagBits (COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT),
    CommandPoolCreateInfo (..),
    ComputePipelineCreateInfo (basePipelineHandle, layout, stage),
    DescriptorBufferInfo (DescriptorBufferInfo),
    DescriptorPool,
    DescriptorPoolCreateFlagBits (..),
    DescriptorPoolCreateInfo (..),
    DescriptorPoolSize (DescriptorPoolSize),
    DescriptorSet,
    DescriptorSetAllocateInfo (descriptorPool, setLayouts),
    DescriptorSetLayout,
    DescriptorSetLayoutBinding
      ( binding,
        descriptorCount,
        descriptorType,
        stageFlags
      ),
    DescriptorSetLayoutCreateInfo (bindings),
    DescriptorType (DESCRIPTOR_TYPE_STORAGE_BUFFER),
    Device (Device, deviceHandle),
    DeviceCreateInfo (..),
    DeviceQueueCreateInfo (queueFamilyIndex, queuePriorities),
    DeviceSize,
    ExtensionProperties (extensionName),
    Instance (Instance, instanceHandle),
    InstanceCreateInfo
      ( applicationInfo,
        enabledExtensionNames,
        enabledLayerNames
      ),
    LayerProperties (layerName),
    MemoryHeap (size),
    MemoryPropertyFlagBits (..),
    MemoryPropertyFlags,
    PhysicalDevice (physicalDeviceHandle),
    PhysicalDeviceFeatures (shaderFloat64, shaderInt16, shaderInt64),
    PhysicalDeviceMemoryProperties (memoryHeaps),
    PhysicalDeviceProperties (apiVersion, deviceName),
    Pipeline,
    PipelineBindPoint (PIPELINE_BIND_POINT_COMPUTE),
    PipelineLayout,
    PipelineLayoutCreateInfo (setLayouts),
    PipelineShaderStageCreateInfo (module', name, stage),
    QueueFamilyProperties (queueCount, queueFlags),
    QueueFlagBits (QUEUE_COMPUTE_BIT),
    Result (TIMEOUT),
    ShaderModule,
    ShaderModuleCreateInfo (code),
    ShaderStageFlagBits (SHADER_STAGE_COMPUTE_BIT),
    SpecializationInfo (..),
    SpecializationMapEntry (..),
    SubmitInfo (commandBuffers),
    WriteDescriptorSet
      ( bufferInfo,
        descriptorCount,
        descriptorType,
        dstBinding,
        dstSet
      ),
    allocateCommandBuffers,
    allocateDescriptorSets,
    beginCommandBuffer,
    cmdBindDescriptorSets,
    cmdBindPipeline,
    cmdCopyBuffer,
    cmdDispatch,
    createCommandPool,
    createComputePipelines,
    createDescriptorPool,
    createDescriptorSetLayout,
    createDevice,
    createFence,
    createInstance,
    createPipelineLayout,
    createShaderModule,
    destroyCommandPool,
    destroyDescriptorPool,
    destroyDescriptorSetLayout,
    destroyDevice,
    destroyFence,
    destroyInstance,
    destroyPipeline,
    destroyPipelineLayout,
    destroyShaderModule,
    endCommandBuffer,
    enumerateInstanceExtensionProperties,
    enumerateInstanceLayerProperties,
    enumeratePhysicalDevices,
    freeCommandBuffers,
    freeDescriptorSets,
    getDeviceQueue,
    getPhysicalDeviceFeatures,
    getPhysicalDeviceMemoryProperties,
    getPhysicalDeviceProperties,
    getPhysicalDeviceQueueFamilyProperties,
    queueSubmit,
    resetCommandBuffer,
    resetFences,
    specializationInfo,
    updateDescriptorSets,
    waitForFences,
    pattern WHOLE_SIZE,
  )
import Vulkan.Core10.ExtensionDiscovery (enumerateDeviceExtensionProperties)
import Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage (PhysicalDevice16BitStorageFeatures (..))
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2
  ( getPhysicalDeviceFeatures2,
  )
import Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage (PhysicalDevice8BitStorageFeatures (..))
import Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64
  ( PhysicalDeviceShaderAtomicInt64Features (..),
  )
import Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8 (PhysicalDeviceShaderFloat16Int8Features (..))
import Vulkan.Core13 (PhysicalDeviceMaintenance4Features (..), pattern API_VERSION_1_3)
import Vulkan.Dynamic
  ( DeviceCmds
      ( DeviceCmds,
        pVkGetDeviceProcAddr
      ),
    InstanceCmds
      ( InstanceCmds,
        pVkGetInstanceProcAddr
      ),
  )
import Vulkan.Exception (VulkanException)
import Vulkan.Extensions.VK_EXT_debug_utils
  ( DebugUtilsMessageSeverityFlagBitsEXT (..),
    DebugUtilsMessageTypeFlagBitsEXT
      ( DEBUG_UTILS_MESSAGE_TYPE_DEVICE_ADDRESS_BINDING_BIT_EXT,
        DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT,
        DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
        DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
      ),
    DebugUtilsMessengerCreateInfoEXT
      ( messageSeverity,
        messageType,
        pfnUserCallback
      ),
    DebugUtilsMessengerEXT,
    createDebugUtilsMessengerEXT,
    destroyDebugUtilsMessengerEXT,
    pattern EXT_DEBUG_UTILS_EXTENSION_NAME,
  )
import Vulkan.Extensions.VK_EXT_shader_atomic_float
  ( PhysicalDeviceShaderAtomicFloatFeaturesEXT (..),
  )
import Vulkan.Extensions.VK_EXT_validation_features (ValidationFeatureEnableEXT (..), ValidationFeaturesEXT (..), pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME)
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (compileShader)
import Vulkan.Zero (Zero (zero))
import VulkanMemoryAllocator as VMA
  ( Allocation,
    AllocationCreateFlags,
    AllocationCreateInfo (flags, requiredFlags, usage),
    AllocationInfo,
    Allocator,
    AllocatorCreateInfo
      ( device,
        flags,
        instance',
        physicalDevice,
        vulkanApiVersion,
        vulkanFunctions
      ),
    MemoryUsage (..),
    VulkanFunctions (vkGetDeviceProcAddr, vkGetInstanceProcAddr),
    createAllocator,
    createBuffer,
    destroyAllocator,
    destroyBuffer,
    flushAllocation,
    invalidateAllocation,
    mapMemory,
    unmapMemory,
  )

-- | Set local thread numbers for a workgroup
localSizeX :: Word32
localSizeY :: Word32
localSizeZ :: Word32
localSizeX = 64

localSizeY = 1

localSizeZ = 1

{-# NOINLINE specializationData #-}
specializationData :: Ptr Word32
specializationData = unsafePerformIO $ newArray [localSizeX, localSizeY, localSizeZ]

-- | Run the compute shader
compute ::
  R.GlobalVulkanResources ->
  R.LocalVulkanResources env ->
  VulkanEnv env' ->
  Env (Idx env') env ->
  [Exists (VulkanArg env)] ->
  [Exists (VulkanArg env)] ->
  [Exists (VulkanArg env)] ->
  Int ->
  IO ()
compute vkGlobal vkLocal env' env inBuffs mutBuffs outBuffs totalThreads
  | inInfo <- getBuffInfo env' env inBuffs,
    mutInfo <- getBuffInfo env' env mutBuffs,
    outInfo <- getBuffInfo env' env outBuffs,
    -- Check that all buffer sizes are greater than 0
    True <- all ((>= 1) . snd) (inInfo ++ mutInfo ++ outInfo) =
      do
        -- Get element byte size and length of each buffer
        let (inElemSizes, inLens) = unzip inInfo
        let (mutElemSizes, mutLens) = unzip mutInfo
        let (outElemSizes, outLens) = unzip outInfo
        let (lockElemSize, lockLen) = case R.initSpinLock vkLocal of
              Nothing -> (0, 0)
              Just (_, _, (len, _)) -> (sizeOf @Int32 undefined, len')
                where
                  len' = product $ map (\idx -> let (Scalar' _ s _) = prj' (prj' idx env) env' in s) len

        -- Get a buffer from the environment
        let getVkBuffer :: VulkanEnv env' -> Env (Idx env') env -> Exists (VulkanArg env) -> ((Buffer, Allocation, AllocationInfo), Ptr ())
            getVkBuffer env' env (Exists (VulkanArg (Var _ idx))) = case prj' (prj' idx env) env' of
              (Buffer' _ _ lt) -> unsafeGetValue lt
              (Scalar' _ _ lt) -> unsafeGetValue lt

        let (inVkBuffers, _) = unzip $ map (getVkBuffer env' env) inBuffs
        -- Create temporary mutable buffers
        let (originalMutVkBuffers, _) = unzip $ map (getVkBuffer env' env) mutBuffs
        newMutVkBuffers <-
          createBufferInfo
            (R.allocator vkGlobal)
            (zipWith (*) mutElemSizes mutLens)
            -- Buffer usages
            (BUFFER_USAGE_STORAGE_BUFFER_BIT .|. BUFFER_USAGE_TRANSFER_SRC_BIT .|. BUFFER_USAGE_TRANSFER_DST_BIT)
            -- Host access pattern
            zero
            -- Memory location
            zero
            -- Memory properties
            MEMORY_PROPERTY_DEVICE_LOCAL_BIT
        let (outVkBuffers, _) = unzip $ map (getVkBuffer env' env) outBuffs
        -- Create a lock buffer
        lockVkBuffer <- case R.initSpinLock vkLocal of
          Nothing -> pure []
          Just _ ->
            createBufferInfo
              (R.allocator vkGlobal)
              [lockElemSize * lockLen]
              BUFFER_USAGE_STORAGE_BUFFER_BIT
              zero
              zero
              MEMORY_PROPERTY_DEVICE_LOCAL_BIT

        -- Assign the buffer in this descriptor set
        let allVkBuffers = inVkBuffers ++ newMutVkBuffers ++ outVkBuffers ++ lockVkBuffer
        let allVkBuffSize =
              map fromIntegral $
                zipWith (*) inElemSizes inLens
                  ++ zipWith (*) mutElemSizes mutLens
                  ++ zipWith (*) outElemSizes outLens
                  ++ [lockElemSize * lockLen | isJust $ R.initSpinLock vkLocal]
        updateDescriptorSets
          (R.dev vkGlobal)
          [ SomeStruct
              zero
                { dstSet = R.descriptorSet vkGlobal,
                  dstBinding = 0,
                  descriptorType = DESCRIPTOR_TYPE_STORAGE_BUFFER,
                  descriptorCount = fromIntegral $ length allVkBuffers,
                  bufferInfo = V.fromList $ zipWith (curry (\((buf, _, _), len) -> DescriptorBufferInfo buf 0 len)) allVkBuffers allVkBuffSize
                }
          ]
          []

        -- Src, dst
        let copyBuffers :: [(Buffer, Allocation, AllocationInfo)] -> [(Buffer, Allocation, AllocationInfo)] -> [Word64] -> IO ()
            copyBuffers [] [] [] = pure ()
            copyBuffers ((srcBuf, _, _) : srcs) ((dstBuf, _, _) : dsts) (len : lens) = do
              cmdCopyBuffer (R.cmdBuffer vkGlobal) srcBuf dstBuf [BufferCopy 0 0 len]
              copyBuffers srcs dsts lens
            copyBuffers _ _ _ = throwString "copyBuffers: src and dst must have the same length"

        -- Fill command buffer
        beginCommandBuffer (R.cmdBuffer vkGlobal) zero {flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}

        -- Fill new mut buffer with original mut buffer
        copyBuffers originalMutVkBuffers newMutVkBuffers (map fromIntegral (zipWith (*) mutElemSizes mutLens))

        case R.initSpinLock vkLocal of
          Nothing -> pure ()
          Just (pipeline, _, _) -> do
            cmdBindPipeline
              (R.cmdBuffer vkGlobal)
              PIPELINE_BIND_POINT_COMPUTE
              pipeline

        cmdBindPipeline
          (R.cmdBuffer vkGlobal)
          PIPELINE_BIND_POINT_COMPUTE
          (R.pipeline vkLocal)
        cmdBindDescriptorSets
          (R.cmdBuffer vkGlobal)
          PIPELINE_BIND_POINT_COMPUTE
          (R.pipelineLayout vkGlobal)
          0
          [R.descriptorSet vkGlobal]
          []
        -- Dispatch the compute shader
        cmdDispatch
          (R.cmdBuffer vkGlobal)
          (ceiling (fromIntegral @_ @Float totalThreads / fromIntegral @_ @Float localSizeX))
          1
          1

        -- Copy new mut buffer back to original mut buffer
        copyBuffers newMutVkBuffers originalMutVkBuffers (map fromIntegral (zipWith (*) mutElemSizes mutLens))

        endCommandBuffer (R.cmdBuffer vkGlobal)

        -- Submit the command buffer and wait for it to execute
        let submitInfo =
              zero {commandBuffers = [commandBufferHandle (R.cmdBuffer vkGlobal)]}
        queueSubmit (R.queue vkGlobal) [SomeStruct submitInfo] (R.fence vkGlobal)

        -- Set time fence
        let fenceTimeout = 1e10 -- 10 second
        waitForFences (R.dev vkGlobal) [R.fence vkGlobal] True fenceTimeout >>= \case
          TIMEOUT -> throwString "Timed out waiting for VK compute"
          _ -> pure ()

        -- Clean up
        resetFences (R.dev vkGlobal) [R.fence vkGlobal]
        resetCommandBuffer (R.cmdBuffer vkGlobal) COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
        destroyVkBuffers (R.allocator vkGlobal) newMutVkBuffers
  | otherwise = pure ()

----------------------------------------------------------------
-- Initialization
----------------------------------------------------------------

myApiVersion :: Word32
myApiVersion = API_VERSION_1_3

-- | Create an instance with a debug messenger
createInstance :: IO (Instance, Maybe DebugUtilsMessengerEXT)
createInstance = do
  availableExtensionNames <-
    toList
      . fmap extensionName
      . snd
      <$> enumerateInstanceExtensionProperties Nothing
  availableLayerNames <-
    toList . fmap layerName . snd <$> enumerateInstanceLayerProperties

  let requiredLayers = []
      -- optionalLayers = ["VK_LAYER_KHRONOS_validation"]
      -- requiredExtensions = [EXT_DEBUG_UTILS_EXTENSION_NAME]
      -- optionalExtensions = [EXT_VALIDATION_FEATURES_EXTENSION_NAME]
      optionalLayers = []
      requiredExtensions = []
      optionalExtensions = []

  extensions <-
    partitionOptReq
      "extension"
      availableExtensionNames
      optionalExtensions
      requiredExtensions
  layers <-
    partitionOptReq
      "layer"
      availableLayerNames
      optionalLayers
      requiredLayers

  let debugMessengerCreateInfo =
        zero
          { messageSeverity =
              DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
            messageType =
              DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
                .|. DEBUG_UTILS_MESSAGE_TYPE_DEVICE_ADDRESS_BINDING_BIT_EXT,
            pfnUserCallback = debugCallbackPtr
          }
      instanceCreateInfo =
        zero
          { applicationInfo =
              Just
                zero
                  { applicationName = Nothing,
                    apiVersion = myApiVersion
                  },
            enabledLayerNames = V.fromList layers,
            enabledExtensionNames = V.fromList extensions
          }
          -- ::& debugMessengerCreateInfo
          -- :& ValidationFeaturesEXT
          --   [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT]
          --   []
          -- :& ()
  inst <- Vulkan.Core10.createInstance instanceCreateInfo Nothing
  -- debugEXT <- createDebugUtilsMessengerEXT inst debugMessengerCreateInfo Nothing
  -- pure (inst, Just debugEXT)
  pure (inst, Nothing)

createDevice :: Instance -> IO (PhysicalDevice, PhysicalDeviceInfo, Device)
createDevice inst = do
  (pdi, phys) <- pickPhysicalDevice inst physicalDeviceInfo
  physProps <- getPhysicalDeviceProperties phys
  say $
    "Using device: "
      <> (decodeUtf8 . deviceName) physProps
      <> ", Vulkan version: "
      <> T.pack (show (apiVersion (physProps :: PhysicalDeviceProperties) `shiftR` 22))
      <> "."
      <> T.pack (show ((apiVersion (physProps :: PhysicalDeviceProperties) `shiftR` 12) .&. 0x3ff))
      <> "."
      <> T.pack (show (apiVersion (physProps :: PhysicalDeviceProperties) .&. 0xfff))

  -- Get device feature supports
  features <- getPhysicalDeviceFeatures phys
  features2 <- getPhysicalDeviceFeatures2 phys
  let _ ::& (maintenance4Features :: PhysicalDeviceMaintenance4Features)
        :& (atomicInt64Features :: PhysicalDeviceShaderAtomicInt64Features)
        :& (atomicFloatFeatures :: PhysicalDeviceShaderAtomicFloatFeaturesEXT)
        :& (shaderF16I8Features :: PhysicalDeviceShaderFloat16Int8Features)
        :& (storage16BitFeatures :: PhysicalDevice16BitStorageFeatures)
        :& (storage8BitFeatures :: PhysicalDevice8BitStorageFeatures)
        :& () = features2

  availableDeviceExtension <-
    toList
      . fmap extensionName
      . snd
      <$> enumerateDeviceExtensionProperties phys Nothing

  -- say $ T.pack $ "Available extensions: " ++ show (map decodeUtf8 availableExtensionNames)

  -- Check for required Vk extensions
  unless (encodeUtf8 "VK_EXT_shader_atomic_float" `elem` availableDeviceExtension) $ throwString "Vulkan device does not support VK_EXT_shader_atomic_float"

  -- Enable a series of Vk extensions
  let extensionEnables = V.fromList $ map encodeUtf8 ["VK_EXT_shader_atomic_float"]

  -- Check for required Vk features
  unless (shaderInt16 features) $ throwString "Vulkan device does not support shaderFloat16"
  unless (shaderInt64 features) $ throwString "Vulkan device does not support shaderInt64"
  unless (shaderFloat64 features) $ throwString "Vulkan device does not support shaderFloat64"

  unless (maintenance4 maintenance4Features) $ throwString "Vulkan device does not support maintenance4"

  unless (shaderBufferInt64Atomics atomicInt64Features) $ throwString "Vulkan device does not support shaderBufferInt64Atomics"
  unless (shaderSharedInt64Atomics atomicInt64Features) $ throwString "Vulkan device does not support shaderSharedInt64Atomics"

  unless (shaderBufferFloat32Atomics atomicFloatFeatures) $ throwString "Vulkan device does not support shaderBufferFloat32Atomics"
  unless (shaderBufferFloat32AtomicAdd atomicFloatFeatures) $ throwString "Vulkan device does not support shaderBufferFloat32AtomicAdd"
  unless (shaderBufferFloat64Atomics atomicFloatFeatures) $ throwString "Vulkan device does not support shaderBufferFloat64Atomics"
  unless (shaderBufferFloat64AtomicAdd atomicFloatFeatures) $ throwString "Vulkan device does not support shaderBufferFloat64AtomicAdd"
  unless (shaderSharedFloat32Atomics atomicFloatFeatures) $ throwString "Vulkan device does not support shaderSharedFloat32Atomics"
  unless (shaderSharedFloat32AtomicAdd atomicFloatFeatures) $ throwString "Vulkan device does not support shaderSharedFloat32AtomicAdd"
  unless (shaderSharedFloat64Atomics atomicFloatFeatures) $ throwString "Vulkan device does not support shaderSharedFloat64Atomics"
  unless (shaderSharedFloat64AtomicAdd atomicFloatFeatures) $ throwString "Vulkan device does not support shaderSharedFloat64AtomicAdd"

  unless (shaderFloat16 shaderF16I8Features) $ throwString "Vulkan device does not support shaderFloat16"
  unless (shaderInt8 shaderF16I8Features) $ throwString "Vulkan device does not support shaderInt8"

  unless (storageBuffer16BitAccess storage16BitFeatures) $ throwString "Vulkan device does not support storageBuffer16BitAccess"
  unless (uniformAndStorageBuffer16BitAccess storage16BitFeatures) $ throwString "Vulkan device does not support uniformAndStorageBuffer16BitAccess"

  unless (storageBuffer8BitAccess storage8BitFeatures) $ throwString "Vulkan device does not support storageBuffer8BitAccess"
  unless (uniformAndStorageBuffer8BitAccess storage8BitFeatures) $ throwString "Vulkan device does not support uniformAndStorageBuffer8BitAccess"

  -- Enable a series of Vk features
  let maintenance4Enables =
        PhysicalDeviceMaintenance4Features
          { maintenance4 = True
          }
      atomicInt64Enables =
        PhysicalDeviceShaderAtomicInt64Features
          { shaderBufferInt64Atomics = True,
            shaderSharedInt64Atomics = True
          }
      atomicFloatEnables =
        PhysicalDeviceShaderAtomicFloatFeaturesEXT
          { shaderBufferFloat32Atomics = True,
            shaderBufferFloat32AtomicAdd = True,
            shaderBufferFloat64Atomics = True,
            shaderBufferFloat64AtomicAdd = True,
            shaderSharedFloat32Atomics = True,
            shaderSharedFloat32AtomicAdd = True,
            shaderSharedFloat64Atomics = True,
            shaderSharedFloat64AtomicAdd = True,
            shaderImageFloat32Atomics = False,
            shaderImageFloat32AtomicAdd = False,
            sparseImageFloat32Atomics = False,
            sparseImageFloat32AtomicAdd = False
          }
      shaderF16I8Enables =
        PhysicalDeviceShaderFloat16Int8Features
          { shaderFloat16 = True,
            shaderInt8 = True
          }
      storage16BitEnables =
        PhysicalDevice16BitStorageFeatures
          { storageBuffer16BitAccess = True,
            uniformAndStorageBuffer16BitAccess = True,
            storagePushConstant16 = False,
            storageInputOutput16 = False
          }
      storage8BitEnables =
        PhysicalDevice8BitStorageFeatures
          { storageBuffer8BitAccess = True,
            uniformAndStorageBuffer8BitAccess = True,
            storagePushConstant8 = False
          }

  let deviceCreateInfo =
        zero
          { queueCreateInfos =
              [ SomeStruct
                  zero
                    { queueFamilyIndex = pdiComputeQueueFamilyIndex pdi,
                      queuePriorities = [1]
                    }
              ],
            enabledFeatures = Just zero {shaderInt16 = True, shaderInt64 = True, shaderFloat64 = True},
            enabledExtensionNames = extensionEnables,
            next = maintenance4Enables :& atomicInt64Enables :& atomicFloatEnables :& shaderF16I8Enables :& storage16BitEnables :& storage8BitEnables :& ()
          }

  dev <- Vulkan.Core10.createDevice phys deviceCreateInfo Nothing
  pure (phys, pdi, dev)

----------------------------------------------------------------
-- Physical device tools
----------------------------------------------------------------

-- | Get the first single PhysicalDevice
pickPhysicalDevice ::
  (MonadIO m, MonadThrow m, Ord a) =>
  Instance ->
  (PhysicalDevice -> m (Maybe a)) ->
  -- A function to get info of PhysicalDevice, Nothing if it is not to be chosen.
  m (a, PhysicalDevice)
pickPhysicalDevice inst getDevInfo = do
  (_, devs) <- enumeratePhysicalDevices inst
  devs' <-
    catMaybes
      <$> sequence [fmap (,d) <$> getDevInfo d | d <- toList devs]
  case devs' of
    [] -> throwString "Unable to find appropriate PhysicalDevice"
    _ -> pure $ head devs' -- Select the first Vulkan compatible device, usually GPU 0

-- | The Ord instance prioritises devices with more memory
--    https://github.com/expipiplus1/vulkan/blob/main/examples/compute/Main.hs
data PhysicalDeviceInfo = PhysicalDeviceInfo
  { pdiTotalMemory :: Word64,
    -- | The queue family index of the first compute queue
    pdiComputeQueueFamilyIndex :: Word32
  }
  deriving (Eq, Ord)

-- https://github.com/expipiplus1/vulkan/blob/main/examples/compute/Main.hs
physicalDeviceInfo :: (MonadIO m) => PhysicalDevice -> m (Maybe PhysicalDeviceInfo)
physicalDeviceInfo phys = runMaybeT $ do
  pdiTotalMemory <- do
    heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
    pure $ sum ((size :: MemoryHeap -> DeviceSize) <$> heaps)
  pdiComputeQueueFamilyIndex <- do
    queueFamilyProperties <- getPhysicalDeviceQueueFamilyProperties phys
    let isComputeQueue q =
          (QUEUE_COMPUTE_BIT .&&. queueFlags q) && (queueCount q > 0)
        computeQueueIndices =
          fromIntegral . fst
            <$> V.filter
              (isComputeQueue . snd)
              (V.indexed queueFamilyProperties)
    MaybeT (pure $ computeQueueIndices V.!? 0)
  pure PhysicalDeviceInfo {..}

-- https://github.com/expipiplus1/vulkan/blob/main/examples/compute/Main.hs
physicalDeviceName :: (MonadIO m) => PhysicalDevice -> m T.Text
physicalDeviceName phys = do
  props <- getPhysicalDeviceProperties phys
  pure $ decodeUtf8 (deviceName props)

----------------------------------------------------------------
-- Resources management
----------------------------------------------------------------

-- | Create global-shared Vk resources across all kernels
createGlobalResources :: Int -> IO R.GlobalVulkanResources
createGlobalResources bufferCount = do
  (inst, debugExt) <- Data.Array.Accelerate.Vulkan.Vulkan.Runtime.createInstance
  (phys, pdi, dev) <- Data.Array.Accelerate.Vulkan.Vulkan.Runtime.createDevice inst
  allocator <-
    createAllocator -- Create a Vk memory allocator
      zero
        { flags = zero,
          physicalDevice = physicalDeviceHandle phys,
          device = deviceHandle dev,
          instance' = instanceHandle inst,
          vulkanApiVersion = myApiVersion,
          vulkanFunctions = Just $ case inst of
            Instance _ InstanceCmds {..} -> case dev of
              Device _ DeviceCmds {..} ->
                zero
                  { vkGetInstanceProcAddr = castFunPtr pVkGetInstanceProcAddr,
                    vkGetDeviceProcAddr = castFunPtr pVkGetDeviceProcAddr
                  }
        }
  (descriptorPool, descriptorSet, descriptorSetLayout) <- createDescriptor dev bufferCount
  pipelineLayout <- Data.Array.Accelerate.Vulkan.Vulkan.Runtime.createPipelineLayout dev descriptorSetLayout
  (cmdPool, cmdBuffer) <- createCommandBuffer dev (pdiComputeQueueFamilyIndex pdi)
  queue <- getDeviceQueue dev (pdiComputeQueueFamilyIndex pdi) 0
  fence <- createFence dev zero Nothing
  pure $ R.GlobalVkResources {..}

-- | Create per kernel local Vk resources
createLocalResources :: R.GlobalVulkanResources -> ByteString -> Maybe (ByteString, ([Idx env Int], Int32)) -> IO (R.LocalVulkanResources env)
createLocalResources vkResources spirvCode mbSpinLock = do
  (shaderInfo, shader) <- createShader (R.dev vkResources) spirvCode
  pipeline <- createComputePipeline (R.dev vkResources) (R.pipelineLayout vkResources) shaderInfo
  initSpinLock <- case mbSpinLock of
    Nothing -> pure Nothing
    Just (spirvCode', info) -> do
      (shaderInfo', shader') <- createShader (R.dev vkResources) spirvCode'
      pipeline' <- createComputePipeline (R.dev vkResources) (R.pipelineLayout vkResources) shaderInfo'
      pure $ Just (pipeline', shader', info)
  pure $ R.LocalVkResources {..}

destroyVkBuffers :: Allocator -> [(Buffer, Allocation, AllocationInfo)] -> IO ()
destroyVkBuffers _ [] = pure ()
destroyVkBuffers allocator ((buffer, allocation, _) : rest) = do
  VMA.destroyBuffer allocator buffer allocation
  destroyVkBuffers allocator rest

destroyGlobalResources :: R.GlobalVulkanResources -> IO ()
destroyGlobalResources vkResources = do
  destroyFence (R.dev vkResources) (R.fence vkResources) Nothing
  freeCommandBuffers (R.dev vkResources) (R.cmdPool vkResources) [R.cmdBuffer vkResources]
  destroyCommandPool (R.dev vkResources) (R.cmdPool vkResources) Nothing
  destroyPipelineLayout (R.dev vkResources) (R.pipelineLayout vkResources) Nothing
  freeDescriptorSets (R.dev vkResources) (R.descriptorPool vkResources) [R.descriptorSet vkResources]
  destroyDescriptorSetLayout (R.dev vkResources) (R.descriptorSetLayout vkResources) Nothing
  destroyDescriptorPool (R.dev vkResources) (R.descriptorPool vkResources) Nothing
  destroyAllocator (R.allocator vkResources)
  destroyDevice (R.dev vkResources) Nothing
  case R.debugExt vkResources of
    Nothing -> pure ()
    Just debugExt -> destroyDebugUtilsMessengerEXT (R.inst vkResources) debugExt Nothing
  destroyInstance (R.inst vkResources) Nothing
  pure ()

destroyLocalResources :: R.GlobalVulkanResources -> R.LocalVulkanResources env -> IO ()
destroyLocalResources vkGlobal vkLocal = do
  destroyPipeline (R.dev vkGlobal) (R.pipeline vkLocal) Nothing
  destroyShaderModule (R.dev vkGlobal) (R.shader vkLocal) Nothing
  case R.initSpinLock vkLocal of
    Nothing -> pure ()
    Just (pipeline, shader, _) -> do
      destroyPipeline (R.dev vkGlobal) pipeline Nothing
      destroyShaderModule (R.dev vkGlobal) shader Nothing

-- | Create buffers from a list of byte sizes
createBufferInfo ::
  Allocator ->
  [Int] ->
  BufferUsageFlags ->
  AllocationCreateFlags ->
  MemoryUsage ->
  MemoryPropertyFlags ->
  IO [(Buffer, Allocation, AllocationInfo)]
createBufferInfo _ [] _ _ _ _ = pure []
createBufferInfo allocator (x : xs) buffUsage alloCreateFlag memUsage memProp = do
  (buffer, allocation, allocationInfo) <-
    if x >= 1
      then do
        catch @IO @VulkanException
          ( VMA.createBuffer
              allocator
              zero
                { size = fromIntegral x,
                  usage = buffUsage
                }
              zero
                { flags = alloCreateFlag,
                  usage = memUsage,
                  requiredFlags = memProp
                }
          )
          $ \_ -> do
            say "Failed to create Vulkan buffer, trying to perform GC"
            performGC
            VMA.createBuffer
              allocator
              zero
                { size = fromIntegral x,
                  usage = buffUsage
                }
              zero
                { flags = alloCreateFlag,
                  usage = memUsage,
                  requiredFlags = memProp
                }
      else do
        say "Warning: buffer size less than 1, skipping this buffer creation"
        pure (zero, zero, zero)
  rest <- createBufferInfo allocator xs buffUsage alloCreateFlag memUsage memProp
  return $ (buffer, allocation, allocationInfo) : rest

-- | Create descriptor bindings for a compute shader given the number of buffers
createDescriptorBindings :: Int -> [DescriptorSetLayoutBinding]
createDescriptorBindings n =
  [ zero
      { binding = fromIntegral i,
        descriptorType = DESCRIPTOR_TYPE_STORAGE_BUFFER,
        descriptorCount = 1,
        stageFlags = SHADER_STAGE_COMPUTE_BIT
      }
    | i <- [0 .. n - 1]
  ]

-- | Create update descriptor sets for a compute shader given a descriptor set and a list of buffers
createUpdateDescriptorSets :: DescriptorSet -> [(Buffer, Allocation, AllocationInfo)] -> Word32 -> [SomeStruct WriteDescriptorSet]
createUpdateDescriptorSets _ [] _ = []
createUpdateDescriptorSets descriptorSet ((buffer, _, _) : rest) bindingIdx =
  SomeStruct
    zero
      { dstSet = descriptorSet,
        dstBinding = bindingIdx,
        descriptorType = DESCRIPTOR_TYPE_STORAGE_BUFFER,
        descriptorCount = 1,
        bufferInfo = [DescriptorBufferInfo buffer 0 WHOLE_SIZE]
      }
    : createUpdateDescriptorSets descriptorSet rest (bindingIdx + 1)

-- | Compile GLSL code to SPIR-V
compileGLSL :: String -> IO ByteString
compileGLSL glslCode =
  do
    -- \| Format output info
    let formatInfoOutput :: String -> [String] -> String
        formatInfoOutput prefix info = fold' prefix info ""
          where
            fold' :: String -> [String] -> String -> String
            fold' _ [] s = s
            fold' p [x] s = s ++ (p ++ x)
            fold' p (x : xs) s = fold' p xs (s ++ (p ++ x) ++ "\n")

    -- Manually assign "--target-env vulkan1.3" for enable LocalSizeId execution mode
    -- See https://github.com/KhronosGroup/Vulkan-ValidationLayers/issues/7394
    (warnings, result) <- compileShader Nothing (Just "vulkan1.3") "comp" Nothing glslCode
    say $ T.pack $ formatInfoOutput "Glslang warning: " warnings
    case result of
      Left err -> throwString $ "GLSL code: \n" ++ glslCode ++ "\n" ++ formatInfoOutput "Failed to compile shader: " err
      Right compCode -> return compCode

-- | Create a shader module from GLSL code
createShader :: Device -> ByteString -> IO (SomeStruct PipelineShaderStageCreateInfo, ShaderModule)
createShader dev spirvCode = do
  -- Create Specialization
  let specializationMapEntries =
        [ SpecializationMapEntry
            { constantID = 0,
              offset = 0,
              size = fromIntegral $ sizeOf (undefined :: Word32)
            },
          SpecializationMapEntry
            { constantID = 1,
              offset = fromIntegral $ sizeOf (undefined :: Word32),
              size = fromIntegral $ sizeOf (undefined :: Word32)
            },
          SpecializationMapEntry
            { constantID = 2,
              offset = 2 * fromIntegral (sizeOf (undefined :: Word32)),
              size = fromIntegral $ sizeOf (undefined :: Word32)
            }
        ]
  let specializationInfo =
        SpecializationInfo
          { mapEntries = specializationMapEntries,
            dataSize = 3 * fromIntegral (sizeOf (undefined :: Word32)),
            data' = castPtr specializationData
          }

  shaderModule <- createShaderModule dev zero {code = spirvCode} Nothing
  let shaderStageCreateInfo =
        zero
          { stage = SHADER_STAGE_COMPUTE_BIT,
            module' = shaderModule,
            name = "main",
            specializationInfo = Just specializationInfo
          }
  pure (SomeStruct shaderStageCreateInfo, shaderModule)

createDescriptor :: Device -> Int -> IO (DescriptorPool, DescriptorSet, DescriptorSetLayout)
createDescriptor dev n = do
  -- Create a descriptor pool
  descriptorPool <-
    createDescriptorPool
      dev
      zero
        { maxSets = 1,
          poolSizes = [DescriptorPoolSize DESCRIPTOR_TYPE_STORAGE_BUFFER 1],
          flags = DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
        }
      Nothing

  -- Create a set layout
  descriptorSetLayout <-
    createDescriptorSetLayout
      dev
      zero
        { bindings = V.fromList (createDescriptorBindings n)
        }
      Nothing

  -- Allocate a descriptor set from the pool with that layout
  [descriptorSet] <-
    allocateDescriptorSets
      dev
      zero
        { descriptorPool = descriptorPool,
          setLayouts = [descriptorSetLayout]
        }
  pure (descriptorPool, descriptorSet, descriptorSetLayout)

createPipelineLayout :: Device -> DescriptorSetLayout -> IO PipelineLayout
createPipelineLayout dev descriptorSetLayout = do
  Vulkan.Core10.createPipelineLayout
    dev
    zero
      { setLayouts = [descriptorSetLayout]
      }
    Nothing

createComputePipeline :: Device -> PipelineLayout -> SomeStruct PipelineShaderStageCreateInfo -> IO Pipeline
createComputePipeline dev pipelineLayout shaderInfo = do
  let pipelineCreateInfo :: ComputePipelineCreateInfo '[]
      pipelineCreateInfo =
        zero
          { layout = pipelineLayout,
            stage = shaderInfo,
            basePipelineHandle = zero
          }
  (_, [computePipeline]) <-
    createComputePipelines
      dev
      zero
      [SomeStruct pipelineCreateInfo]
      Nothing
  pure computePipeline

createCommandBuffer :: Device -> Word32 -> IO (CommandPool, CommandBuffer)
createCommandBuffer dev computeQueueFamilyIndex = do
  let commandPoolCreateInfo :: CommandPoolCreateInfo
      commandPoolCreateInfo =
        zero
          { queueFamilyIndex = computeQueueFamilyIndex,
            flags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
          }
  commandPool <- Vulkan.Core10.createCommandPool dev commandPoolCreateInfo Nothing
  let commandBufferAllocateInfo =
        zero
          { commandPool = commandPool,
            level = COMMAND_BUFFER_LEVEL_PRIMARY,
            commandBufferCount = 1
          }
  [commandBuffer] <- Vulkan.Core10.allocateCommandBuffers dev commandBufferAllocateInfo
  pure (commandPool, commandBuffer)

----------------------------------------------------------------
-- Data transfer
----------------------------------------------------------------

-- | Get element byte size and length of each buffer
getBuffInfo :: VulkanEnv env' -> Env (Idx env') env -> [Exists (VulkanArg env)] -> [(Int, Int)]
getBuffInfo _ _ [] = []
getBuffInfo env' env (exist : rest) =
  case exist of
    (Exists (VulkanArg (Var (GroundRbuffer st) idx))) -> (elmSize, len) : getBuffInfo env' env rest
      where
        elmSize = bytesElt (TupRsingle st)
        len = case prj' (prj' idx env) env' of
          Buffer' _ (AAB.Buffer n _) _ -> n
          _ -> error "getBuffInfo: Impossible"
    (Exists (VulkanArg (Var (GroundRscalar st) _))) -> (bytesElt (TupRsingle st), 1) : getBuffInfo env' env rest

-- | Wrap types to avoid unsuppoted (Vec n a)
newtype Ptr' a = Ptr' (Ptr (ScalarArrayDataR a))

-- | Copy data stored in a Buffer to a Ptr
copyBufferToPtr :: forall a. ScalarType a -> AAB.Buffer a -> Ptr' a -> IO ()
copyBufferToPtr (SingleScalarType (st :: SingleType a')) (AAB.Buffer size ua) (Ptr' destPtr)
  | SingleDict <- singleDict st,
    SingleArrayDict <- singleArrayDict st =
      do
        -- say $ T.pack $ "copyBufferToPtr: Buffer: " ++ show (bufferToList (SingleScalarType st) size (AAB.Buffer size ua))  -- !NOTE: for debugging, may decrease performance
        withUniqueArrayPtr ua $ \srcPtr -> copyArray (destPtr :: Ptr a) srcPtr size
-- liftIO $ peekArray size (destPtr :: Ptr a) >>= \x -> say $ T.pack $ "copyBufferToPtr: Ptr: " ++ show x  -- !NOTE: for debugging, may decrease performance
copyBufferToPtr (VectorScalarType {}) _ _ = error "copyBufferToPtr: VectorScalarType not supported"

-- | Copy a scalar to a Ptr
copyScalarToPtr :: forall a. ScalarType a -> a -> Ptr' a -> IO ()
copyScalarToPtr (SingleScalarType (st :: SingleType a')) scalar (Ptr' destPtr)
  | SingleDict <- singleDict st,
    SingleArrayDict <- singleArrayDict st =
      do
        -- say $ T.pack $ "copyScalarToPtr: Scalar: " ++ show scalar  -- !NOTE: for debugging, may decrease performance
        poke (destPtr :: Ptr a) scalar
-- liftIO $ peek destPtr >>= \x -> say $ T.pack $ "copyScalarToPtr: Ptr: " ++ show x  -- !NOTE: for debugging, may decrease performance
copyScalarToPtr _ _ _ = error "copyScalarToPtr: Impossible"

-- | Copy data stored in a Ptr to a Buffer
copyPtrToBuffer :: forall a. ScalarType a -> AAB.Buffer a -> Ptr' a -> IO ()
copyPtrToBuffer (SingleScalarType (st :: SingleType a')) (AAB.Buffer size ua) (Ptr' srcPtr)
  | SingleDict <- singleDict st,
    SingleArrayDict <- singleArrayDict st =
      do
        -- liftIO $ peekArray size (srcPtr :: Ptr a) >>= \x -> say $ T.pack $ "copyPtrToBuffer: Ptr: " ++ show x  -- !NOTE: for debugging, may decrease performance
        withUniqueArrayPtr ua $ \destPtr -> copyArray destPtr (srcPtr :: Ptr a) size
-- say $ T.pack $ "copyPtrToBuffer: Buffer: " ++ show (bufferToList (SingleScalarType st) size (AAB.Buffer size ua))  -- !NOTE: for debugging, may decrease performance
copyPtrToBuffer (VectorScalarType {}) _ _ = error "copyPtrToBuffer: VectorScalarType not supported"

-- | Copy a scalar stored in a Ptr to a scalar
copyPtrToScalar :: forall a. ScalarType a -> Ptr' a -> IO a
copyPtrToScalar (SingleScalarType (st :: SingleType a')) (Ptr' srcPtr)
  | SingleDict <- singleDict st,
    SingleArrayDict <- singleArrayDict st =
      do
        -- liftIO $ peek srcPtr >>= \x -> say $ T.pack $ "copyPtrToScalar: Ptr: " ++ show x  -- !NOTE: for debugging, may decrease performance
        peek srcPtr
copyPtrToScalar _ _ = error "copyPtrToScalar: Impossible"

-- | Copy host data to device, given a list of VK buffers, environment, a list of VulkanArg, and a list of sizes
copyHostDataToDevice :: Allocator -> [(Buffer, Allocation, AllocationInfo)] -> VulkanEnv env -> [Int] -> [Exists (VulkanArg env)] -> IO ()
copyHostDataToDevice _ [] _ [] [] = pure ()
copyHostDataToDevice allo ((_, allocation, _) : rest) env (size : sizes) (exist : rest') = do
  case exist of
    (Exists (VulkanArg (Var (GroundRbuffer st) idx))) -> do
      let buffer = case prj' idx env of
            Scalar' {} -> error "copyHostDataToDevice: Impossible"
            Buffer' _ b _ -> b
      -- Start mapping the device memory
      -- See https://hackage.haskell.org/package/VulkanMemoryAllocator-0.11.0.1/docs/VulkanMemoryAllocator.html#v:mapMemory
      elemStartAddr <- mapMemory allo allocation
      copyBufferToPtr st buffer (Ptr' $ castPtr elemStartAddr)
      -- Make the host write push to the device
      flushAllocation allo allocation 0 WHOLE_SIZE
      -- Unmap the device memory
      unmapMemory allo allocation
      copyHostDataToDevice allo rest env sizes rest'
    (Exists (VulkanArg (Var (GroundRscalar st) idx))) -> do
      let scalar = case prj' idx env of
            Scalar' _ d _ -> d
            Buffer' {} -> error "copyHostDataToDevice: Impossible"
      elemStartAddr <- mapMemory allo allocation
      copyScalarToPtr st scalar (Ptr' $ castPtr elemStartAddr)
      flushAllocation allo allocation 0 WHOLE_SIZE
      unmapMemory allo allocation
      copyHostDataToDevice allo rest env sizes rest'
copyHostDataToDevice _ _ _ _ _ = throwString "copyHostDataToDevice: Mismatched buffer and data counts"

-- | Fetch data from the device back to the host buffer
copyDeviceDataToHost :: Lifetime GlobalVulkanResources -> VulkanElement a -> IO a
copyDeviceDataToHost vkGlobalResources (Buffer' st b@(AAB.Buffer _ _) vkBuffer) = do
  withLifetime vkGlobalResources $ \vkGlobal -> do
    withLifetime vkBuffer $ \((_, allocation, _), elemStartAddr) -> do
      -- Make the device write push to the host
      invalidateAllocation (R.allocator vkGlobal) allocation 0 WHOLE_SIZE
      copyPtrToBuffer st b (Ptr' $ castPtr elemStartAddr)
  touchLifetime vkBuffer
  touchLifetime vkGlobalResources
  pure b
copyDeviceDataToHost _ (Scalar' _ s _) = pure s

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- https://github.com/expipiplus1/vulkan/blob/main/examples/compute/Main.hs
partitionOptReq ::
  (Show a, Eq a, MonadIO m) => T.Text -> [a] -> [a] -> [a] -> m [a]
partitionOptReq type' available optional required = do
  let (optHave, optMissing) = partition (`elem` available) optional
      (reqHave, reqMissing) = partition (`elem` available) required
      tShow = T.pack . show
  for_ optMissing $
    \n -> sayErr $ "Missing optional " <> type' <> ": " <> tShow n
  case reqMissing of
    [] -> pure ()
    [x] -> sayErr $ "Missing required " <> type' <> ": " <> tShow x
    xs -> sayErr $ "Missing required " <> type' <> "s: " <> tShow xs
  pure (reqHave <> optHave)

----------------------------------------------------------------
-- Bit utils
----------------------------------------------------------------

-- https://github.com/expipiplus1/vulkan/blob/main/examples/compute/Main.hs
(.&&.) :: (Bits a) => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)
