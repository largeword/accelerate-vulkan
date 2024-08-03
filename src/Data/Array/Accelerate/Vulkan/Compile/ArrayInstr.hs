{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Array.Accelerate.Vulkan.Compile.ArrayInstr where

import Control.Monad.State (MonadState (get, put), State)
import Data.Array.Accelerate.AST.Environment (partialUpdate, prjPartial)
import Data.Array.Accelerate.AST.Idx (idxToInt)
import Data.Array.Accelerate.AST.Operation (ArrayInstr (..))
import Data.Array.Accelerate.AST.Var (Var (..))
import Data.Array.Accelerate.Representation.Ground (GroundR (..))
import Data.Array.Accelerate.Vulkan.Common (VulkanArg (..))
import Data.Array.Accelerate.Vulkan.Compile.Type
import Prelude hiding (exp, init, lookup)

compileArrayInstr :: ArrayInstr benv t -> State (VarCount, FuncMap, AInstrEnv benv) (String, VarName t)
compileArrayInstr (Index arrBuff@(Var (GroundRbuffer _) idx)) =
  do
    (varCount, funcMap, aInstrEnv) <- get
    -- The bigger the number is, the deeper the idx points to the environment
    let idxNum = idxToInt idx
    case prjPartial idx aInstrEnv of
      Just (AInstr _ (VulkanArg (Var (GroundRscalar _) _))) -> error "compileArrayInstr: GroundRbuffer impossible"
      Just (AInstr arrName (VulkanArg (Var (GroundRbuffer _) _))) -> return ("", VarName arrName)
      Nothing -> do
        let arrName = "aInstr" ++ show idxNum
        let vkArg = AInstr arrName (VulkanArg arrBuff)
        let aInstrEnv' = partialUpdate vkArg idx aInstrEnv
        put (varCount, funcMap, aInstrEnv')
        return ("", VarName arrName)
compileArrayInstr (Parameter (Var st idx)) =
  do
    (varCount, funcMap, aInstrEnv) <- get
    let idxNum = idxToInt idx
    case prjPartial idx aInstrEnv of
      Just (AInstr arrName _) -> return ("", VarName arrName)
      Nothing -> do
        let arrName = "aInstr" ++ show idxNum
        let vkArg = AInstr arrName (VulkanArg (Var (GroundRscalar st) idx))
        let aInstrEnv' = partialUpdate vkArg idx aInstrEnv
        put (varCount, funcMap, aInstrEnv')
        return ("", VarName arrName)
compileArrayInstr _ = error "compileArrayInstr: Impossible"
