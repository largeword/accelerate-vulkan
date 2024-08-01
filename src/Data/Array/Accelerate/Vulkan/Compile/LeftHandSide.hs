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

module Data.Array.Accelerate.Vulkan.Compile.LeftHandSide where

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


-- | Compile LeftHandSide and ExpStringTup into (Statement, new VarNameEnv) given old VarNameEnv
compileLhs :: VarNameEnv env -> ELeftHandSide bnd_t env env' -> ExpStringTup bnd_t -> State (VarCount, FuncMap, AInstrEnv benv) (String, VarNameEnv env')
compileLhs env (LeftHandSideWildcard _) _ = return ("", env)
compileLhs env (LeftHandSideSingle st) (TupRsingle (ExpString exp))
  = do
    (letStatement, letVars) <- newAndBindVars (TupRsingle st) "let" (TupRsingle (ExpString exp))
    let letVarName = case letVars of
          (TupRsingle (VarName v)) -> (VarName v)
          _ -> error "compileLhs: letVarName impossible"
    -- Add new let-binding variable to env
    let newEnv = Push env letVarName
    return (letStatement, newEnv)
compileLhs env (LeftHandSidePair lLhs rLhs) (TupRpair lExp rExp)
  = do
      (lStatement, lEnv) <- compileLhs env lLhs lExp
      (rStatement, rEnv) <- compileLhs lEnv rLhs rExp
      return (lStatement ++ rStatement, rEnv)
compileLhs _ lhs exps = error $ "compileLhs: LeftHandSide (" ++ show lhs ++ ") and Expressions (" ++ show exps ++ ") do not match"

-- | Add existing vars to env according to LHS
compileVarsLhs :: Env f env -> ELeftHandSide bnd_t env env' -> TupR f bnd_t -> Env f env'
compileVarsLhs env (LeftHandSideWildcard _) _ = env
compileVarsLhs env (LeftHandSideSingle _) (TupRsingle var) = Push env var
compileVarsLhs env (LeftHandSidePair lLhs rLhs) (TupRpair lExp rExp) = compileVarsLhs (compileVarsLhs env lLhs lExp) rLhs rExp
compileVarsLhs _ lhs _ = error $ "compileVarsLhs: LeftHandSide (" ++ show lhs ++ ") and TupR do not match"