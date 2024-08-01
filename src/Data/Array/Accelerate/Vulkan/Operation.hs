{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Array.Accelerate.Vulkan.Operation where

import Data.Array.Accelerate.AST.Operation
  ( Arg,
    Fun',
    In,
    Mut,
    OperationAcc,
    Out,
    PreArgs (ArgsNil, (:>:)),
    PreOpenAcc (..),
    PrimMaybe,
  )
import Data.Array.Accelerate.Analysis.Hash.Exp
  ( hashQ,
    intHost,
  )
import Data.Array.Accelerate.Analysis.Hash.Operation
  ( EncodeOperation (..),
  )
import Data.Array.Accelerate.Backend
  ( DesugarAcc (mkGenerate, mkPermute),
    MakesILP (..),
    NFData' (..),
    PrettyOp (prettyOp),
    SLVOperation (..),
    ShrinkArg (..),
    SimplifyOperation (..),
  )
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Graph
  ( LabelledArgOp (LOp),
    finalizeNoFusion,
  )
import Data.Array.Accelerate.Trafo.Partitioning.ILP.Labels
  ( LabelledArg (L),
  )

-- Make constructors for Vulkan ops
data VulkanOp t where
  VkGenerate :: VulkanOp (Fun' (sh -> t) -> Out sh t -> ())
  VkPermute :: VulkanOp (Fun' (e -> e -> e) -> Mut sh' e -> Fun' (sh -> PrimMaybe sh') -> In sh e -> ())

instance PrettyOp VulkanOp where
  prettyOp VkGenerate = "VkGenerate"
  prettyOp VkPermute = "VkPermute"

instance Show (VulkanOp t) where
  show = show . prettyOp

instance EncodeOperation VulkanOp where
  encodeOperation VkGenerate = intHost $(hashQ ("VkGenerate" :: String))
  encodeOperation VkPermute = intHost $(hashQ ("VkPermute" :: String))

instance NFData' VulkanOp where
  rnf' !_ = ()

instance SimplifyOperation VulkanOp where
  detectCopy _ _ _ = []

instance SLVOperation VulkanOp where
  slvOperation _ = Nothing

instance MakesILP VulkanOp where
  type BackendVar VulkanOp = ()
  type BackendArg VulkanOp = ()
  defaultBA = ()
  data BackendClusterArg VulkanOp arg = NoFusionArg
  combineBackendClusterArg _ _ = NoFusionArg
  encodeBackendClusterArg NoFusionArg = intHost $(hashQ ("NoFusionArg" :: String))
  mkGraph _ _ _ = mempty
  labelLabelledArg _ _ (L arg l) = LOp arg l ()
  getClusterArg (LOp _ _ ()) = NoFusionArg
  finalize = finalizeNoFusion

instance NFData' (BackendClusterArg VulkanOp) where
  rnf' NoFusionArg = ()

instance ShrinkArg (BackendClusterArg VulkanOp) where
  shrinkArg _ NoFusionArg = NoFusionArg
  deadArg NoFusionArg = NoFusionArg

instance DesugarAcc VulkanOp where
  mkGenerate :: Arg env (Fun' (sh -> t)) -> Arg env (Out sh t) -> OperationAcc VulkanOp env ()
  mkGenerate f aOut = Exec VkGenerate (f :>: aOut :>: ArgsNil)

  mkPermute :: Arg env (Fun' (e -> e -> e)) -> Arg env (Mut sh' e) -> Arg env (Fun' (sh -> PrimMaybe sh')) -> Arg env (In sh e) -> OperationAcc VulkanOp env ()
  mkPermute combF aMut idxF aIn = Exec VkPermute (combF :>: aMut :>: idxF :>: aIn :>: ArgsNil)
