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

module Data.Array.Accelerate.Vulkan.Compile.Env where

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

type VarCount = Int

-- | Store variable name
newtype VarName t = VarName String
type VarNameTup t = TupR VarName t
type VarNameEnv env = Env VarName env

instance Show (VarName t) where
  show (VarName s) = s
instance Show (VarNameEnv env) where
  show (Empty) = "Empty"
  show (Push env' (VarName v)) = "(" ++ show env' ++ ", " ++ v ++ ")"

-- | Store compiled expression
newtype ExpString t = ExpString String
type ExpStringTup t = TupR ExpString t
type ExpStringEnv env = Env ExpString env

instance Show (ExpString t) where
  show (ExpString s) = s

-- | Store GLSL function names and their definitions
--    with function name as key, and definition as value
--    functions are inserted by order of dependency
type FuncMap = OMap String String

-- -- | Store args constructed by ArrayInstr
-- --    with integer index of benv as key, and (argName, VulkanArg benv a) as value
-- type AInstrMap benv = OMap Int (String, (Exists (VulkanArg benv)))
  
-- | Store array instructions with its name ("aInstr" ++ show $ idxToInt idx)
data AInstr benv t = AInstr String (VulkanArg benv t)
type AInstrEnv benv = PartialEnv (AInstr benv) benv

instance Show (AInstr benv t) where
  show (AInstr s _) = s


-- | Convert VarNameTup to ExpStringTup
convertVarName2ExpString :: VarNameTup t -> ExpStringTup t
convertVarName2ExpString TupRunit = TupRunit
convertVarName2ExpString (TupRsingle (VarName v)) = TupRsingle $ ExpString v
convertVarName2ExpString (TupRpair l r) = TupRpair (convertVarName2ExpString l) (convertVarName2ExpString r)

env2TupR :: Env f env -> TupR f env
env2TupR Empty = TupRunit
env2TupR (Push env v) = TupRpair (env2TupR env) (TupRsingle v)

tupR2Env :: TupR f env -> Env f env
tupR2Env TupRunit = Empty
tupR2Env (TupRpair l (TupRsingle r)) = Push (tupR2Env l) r
tupR2Env _ = error "tupR2Env: Conversion impossible"



-- | Update VarNameEnv according to LeftHandSide and ExpStringTup
updateVarNameEnv :: VarNameEnv env -> (LeftHandSide s t env env', ExpStringTup t) -> VarNameEnv env'
updateVarNameEnv env (LeftHandSideWildcard _, _) = env
updateVarNameEnv env (LeftHandSideSingle _, TupRsingle (ExpString v)) = Push env (VarName v)
updateVarNameEnv env (LeftHandSidePair l r, TupRpair l' r') = updateVarNameEnv (updateVarNameEnv env (l, l')) (r, r')
updateVarNameEnv _ _ = error "updateVarNameEnv impossible"