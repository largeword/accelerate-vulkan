{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Array.Accelerate.Vulkan.Compile.Type where

import Data.Array.Accelerate.AST.Environment (Env (Empty, Push), PartialEnv (..))
import Data.Array.Accelerate.AST.LeftHandSide (LeftHandSide (..))
import Data.Array.Accelerate.Representation.Type (TupR (..))
import Data.Array.Accelerate.Vulkan.Common (VulkanArg (..))
import Data.Map.Ordered (OMap)
import Prelude hiding (exp, init, lookup)

type VarCount = Int

-- | Store variable name
newtype VarName t = VarName String

type VarNameTup t = TupR VarName t

type VarNameEnv env = Env VarName env

instance Show (VarName t) where
  show (VarName s) = s

instance Show (VarNameEnv env) where
  show Empty = "Empty"
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
