{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Array.Accelerate.Vulkan.Compile.LeftHandSide where

import Control.Monad.State (State)
import Data.Array.Accelerate.AST.Environment (Env (Push))
import Data.Array.Accelerate.AST.Exp (ELeftHandSide)
import Data.Array.Accelerate.AST.LeftHandSide (LeftHandSide (..))
import Data.Array.Accelerate.Representation.Type (TupR (..))
import Data.Array.Accelerate.Vulkan.Compile.Type
  ( AInstrEnv,
    ExpString (ExpString),
    ExpStringTup,
    FuncMap,
    VarCount,
    VarName (VarName),
    VarNameEnv,
  )
import Data.Array.Accelerate.Vulkan.Compile.Var (newAndBindVars)
import Prelude hiding (exp, init, lookup)

-- | Compile LeftHandSide and ExpStringTup into (Statement, new VarNameEnv) given old VarNameEnv
compileLhs :: VarNameEnv env -> ELeftHandSide bnd_t env env' -> ExpStringTup bnd_t -> State (VarCount, FuncMap, AInstrEnv benv) (String, VarNameEnv env')
compileLhs env (LeftHandSideWildcard _) _ = return ("", env)
compileLhs env (LeftHandSideSingle st) (TupRsingle (ExpString exp)) =
  do
    (letStatement, letVars) <- newAndBindVars (TupRsingle st) "let" (TupRsingle (ExpString exp))
    let letVarName = case letVars of
          (TupRsingle (VarName v)) -> VarName v
          _ -> error "compileLhs: letVarName impossible"
    -- Add new let-binding variable to env
    let newEnv = Push env letVarName
    return (letStatement, newEnv)
compileLhs env (LeftHandSidePair lLhs rLhs) (TupRpair lExp rExp) =
  do
    (lStatement, lEnv) <- compileLhs env lLhs lExp
    (rStatement, rEnv) <- compileLhs lEnv rLhs rExp
    return (lStatement ++ rStatement, rEnv)
compileLhs _ lhs exps = error $ "compileLhs: LeftHandSide (" ++ show lhs ++ ") and Expressions (" ++ show exps ++ ") do not match"
