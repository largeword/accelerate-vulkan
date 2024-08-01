{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Array.Accelerate.Vulkan.Compile.ArrayInstr where

import Prelude hiding (exp, lookup, init)

import Data.Array.Accelerate.AST.Environment
    ( Env(Push, Empty), prj', PartialEnv (..), prjPartial, partialUpdate )
import Data.Array.Accelerate.Representation.Shape (ShapeR (..))
import Data.Array.Accelerate.Representation.Type
    ( TupR(..), TypeR )
import Data.Array.Accelerate.Type
    ( ScalarType(..),
      FloatingType(..),
      IntegralType(..),
      NumType(..),
      SingleType(..), BitSizeEq, IntegralDict (..), integralDict, FloatingDict (..), floatingDict, BoundedType (..) )
import Data.Array.Accelerate.AST.LeftHandSide (LeftHandSide(..), Exists(..))
import Data.Array.Accelerate.AST.Operation (ArrayInstr (..), OpenExp, Fun)

import Control.Monad.State ( State, MonadState(put, get) )
import Data.Map.Ordered (OMap, notMember, lookup, (|>))
import Data.Array.Accelerate.Representation.Elt (bytesElt)
import Data.Array.Accelerate.Vulkan.Type (VulkanArg (..))
import Data.Array.Accelerate.AST.Exp (PreOpenExp (..), PrimFun (..), expType, IsArrayInstr (..), PrimConst (..), PreOpenFun (..), TAG)
import Data.Array.Accelerate.AST.Exp (ELeftHandSide)
import Data.Array.Accelerate.AST.Var (Var(..))
import Data.Array.Accelerate.AST.Idx (idxToInt, Idx (..))
import Data.Array.Accelerate.Representation.Ground (GroundR(..))
import Data.Array.Accelerate.AST.Schedule.Uniform (BaseR(BaseRground))
import Data.Array.Accelerate.Representation.Slice (SliceIndex (..))
import Data.Array.Accelerate.Trafo.Exp.Substitution (rebuildNoArrayInstr)

import Data.Array.Accelerate.Vulkan.Compile.Env
import Data.Array.Accelerate.Vulkan.Compile.Var


-- -- | Compile ArrayInstr into (Statement, TupR Expression)
-- --    add new VulkanArg to AInstrMap
-- compileArrayInstr :: ArrayInstr benv t -> State (VarCount, FuncMap, AInstrMap benv) (String, VarName t)
-- compileArrayInstr (Index (Var groundR@(GroundRbuffer arrType) idx))
--   = do
--     (varCount, funcMap, aInstrMap) <- get
--     let idxNum = idxToInt idx
--     -- Check if the called array is already in AInstrMap
--     case lookup idxNum aInstrMap of
--       Just (arrName, _) -> return ("", VarName arrName)
--       Nothing -> do
--         let arrName = "aInstr" ++ show idxNum
--         let vkArg = Exists $ ArrayArg arrType (Var (BaseRground groundR) idx)
--         let aInstrMap' = aInstrMap |> (idxNum, (arrName, vkArg))
--         put (varCount, funcMap, aInstrMap')
--         return ("", VarName arrName)
-- compileArrayInstr (Parameter (Var st idx))
--   = do
--     (varCount, funcMap, aInstrMap) <- get
--     let idxNum = idxToInt idx
--     -- Check if the called array is already in AInstrMap
--     case lookup idxNum aInstrMap of
--       Just (sName, _) -> return ("", VarName sName)
--       Nothing -> do
--         let sName = "aInstr" ++ show idxNum
--         let vkArg = Exists $ ScalarArg (Var st idx)
--         let aInstrMap' = aInstrMap |> (idxNum, (sName, vkArg))
--         put (varCount, funcMap, aInstrMap')
--         return ("", VarName sName)
-- compileArrayInstr _ = error "compileArrayInstr: Impossible"

compileArrayInstr :: ArrayInstr benv t -> State (VarCount, FuncMap, AInstrEnv benv) (String, VarName t)
compileArrayInstr (Index arrBuff@(Var (GroundRbuffer _) idx))
  = do
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
compileArrayInstr (Parameter (Var st idx))
  = do
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