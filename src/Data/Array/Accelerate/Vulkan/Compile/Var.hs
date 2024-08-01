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

module Data.Array.Accelerate.Vulkan.Compile.Var where

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
import Data.Array.Accelerate.Vulkan.Compile.Convert

-- | Create new variables according to TypeR given suffix, return statement and TupR new variables
newVars :: TypeR t -> String -> State (VarCount, FuncMap, AInstrEnv benv) (String, VarNameTup t)
newVars TupRunit _ = return ("", TupRunit)
newVars (TupRsingle st) suffix = do
  -- Use global state to create a new return variable
  (varCount, funcMap, aInstrMap) <- get
  let varName = "e" ++ show varCount ++ "_" ++ suffix
  let varTp = scalarTypeToString st
  put (varCount + 1, funcMap, aInstrMap)
  return (varTp ++ " " ++ varName ++ ";\n", TupRsingle $ VarName varName)
newVars (TupRpair l r) suffix = do
  (lStatement, lExp) <- newVars l suffix
  (rStatement, rExp) <- newVars r suffix
  return (lStatement ++ rStatement, TupRpair lExp rExp)

-- | Bind new variables according to ExpStringTup, return statement
bindNewVars :: VarNameTup t -> ExpStringTup t -> String
bindNewVars TupRunit TupRunit = ""
bindNewVars (TupRsingle (VarName varName)) (TupRsingle (ExpString exp))
  = varName ++ " = " ++ exp ++ ";\n"  -- Bind return variable with expression
bindNewVars (TupRpair l1 r1) (TupRpair l2 r2) = bindNewVars l1 l2 ++ bindNewVars r1 r2
bindNewVars _ _ = error "bindNewVars impossible"

-- | Create new variables given suffix and initialize them with ExpStringTup, return statement and TupR new variables
newAndBindVars :: TypeR t -> String -> ExpStringTup t -> State (VarCount, FuncMap, AInstrEnv benv) (String, VarNameTup t)
newAndBindVars TupRunit _ TupRunit = return ("", TupRunit)
newAndBindVars (TupRsingle st) suffix (TupRsingle (ExpString exp)) = do
  (varCount, funcMap, aInstrMap) <- get
  let varName = "e" ++ show varCount ++ "_" ++ suffix
  let varTp = scalarTypeToString st
  put (varCount + 1, funcMap, aInstrMap)
  return (varTp ++ " " ++ varName ++ " = " ++ exp ++ ";\n", TupRsingle $ VarName varName)
newAndBindVars (TupRpair l r) suffix (TupRpair l' r') = do
  (lStatement, lExp) <- newAndBindVars l suffix l'
  (rStatement, rExp) <- newAndBindVars r suffix r'
  return (lStatement ++ rStatement, TupRpair lExp rExp)
newAndBindVars tr _ exps = error $ "newAndBindVars: TypeR (" ++ show tr ++ ") and Expressions (" ++ show exps ++ ") do not match"

-- | Update vars given expressions, return statement
updateVars :: VarNameTup a -> ExpStringTup a -> String
updateVars TupRunit TupRunit = ""
updateVars (TupRsingle (VarName v1)) (TupRsingle (ExpString v2)) = v1 ++ " = " ++ v2 ++ ";\n"
updateVars (TupRpair v1 v2) (TupRpair e1 e2) = s1 ++ s2
  where s1 = updateVars v1 e1
        s2 = updateVars v2 e2
updateVars vars exps = error $ "updateVars: Vars (" ++ show vars ++ ") and Expressions (" ++ show exps ++ ") do not match"
