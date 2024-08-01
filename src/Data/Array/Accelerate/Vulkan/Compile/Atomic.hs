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

module Data.Array.Accelerate.Vulkan.Compile.Atomic where

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
import Data.Array.Accelerate.AST.Idx (Idx (..))
import Data.Array.Accelerate.Vulkan.Compile.Env
import Data.Array.Accelerate.Vulkan.Compile.Convert
import Data.Array.Accelerate.Vulkan.Compile.Expression
import Data.Array.Accelerate.Vulkan.Compile.Var


-- | Mark if the input type is casted to supported type for atomic compare-and-swap
type IsBitsCasted = Bool

-- | Try to compile Atomic ops, returns Maybe statement
compileAtomic :: ExpStringEnv (((),t),t) -> TypeR (((),t),t) -> OpenExp (((),t),t) benv a -> State (VarCount, FuncMap, AInstrEnv benv) (Maybe String, IsBitsCasted)
compileAtomic env _ (PrimApp f xs)  -- Compile ops that can be atomic directly
  | Just statement <- compileAtomicPrimFun f env xs
  = return (Just statement, False)

-- Compile ops that can be emulated by compare-and-swap (CAS)
-- https://en.wikipedia.org/wiki/Compare-and-swap
-- https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html
compileAtomic env@(Push _ (ExpString mutAccess)) envTypeR exp
  | True <- checkAtomicCompSwapTypeSupport $ expType exp
  = do
    -- Create new vars to store perm value and mut buffer access
    (newStatement, newVar) <- newVars envTypeR "CASin"
    -- Bind new vars with existing ExpStringEnv
    let bndVars = bindNewVars newVar (env2TupR env)
    -- Get old value reference of mut buffer
    let mutOld = case newVar of
          TupRpair _ (TupRsingle (VarName v)) -> v
          _ -> error "compileAtomic: Impossible"
    -- Create new vars to store compare value which equals to old value
    (compStatement, compVar) <- newVars (expType exp) "CAScomp"
    let comp = case compVar of
          TupRsingle (VarName v) -> v
          _ -> error "compileAtomic: Impossible"
    -- Create new VarNameEnv according to newVar
    let newEnv = tupR2Env newVar
    -- Compile expression to get new value for mut buffer
    (updateStatement, updateExp) <- compileStatement newEnv exp
    -- Get new value expression
    let mutNew = case updateExp of
          TupRsingle (ExpString exp') -> exp'
          _ -> error "compileAtomic: Impossible"
    -- Build atomicCAS statement
    let atomicCAS = mutOld ++ " = atomicCompSwap(" ++ mutAccess ++ ", " ++ comp ++ ", " ++ mutNew ++ ");\n"
    -- Build loop statement
    let loopStatement = "do {\n" 
                          ++ bndVars
                          ++ comp ++ " = " ++ mutOld ++ ";\n" 
                          ++ updateStatement
                          ++ atomicCAS 
                          ++ "} while (" ++ mutOld ++ " != " ++ comp ++ ");\n"
    let casStatement = newStatement ++ compStatement ++ loopStatement
    return (Just casStatement, False)

-- | Use bit-preserved conversion to convert unsupported type into supported type for atomic compare-and-swap
compileAtomic (Push env' (ExpString mutAccess)) envTypeR@(TupRpair _ (TupRsingle (SingleScalarType (NumSingleType nt)))) exp
  | True <- checkConvertToAtomicCASTypeSupport $ expType exp
  = do
    -- Create new vars to store perm value and mut buffer access
    (newStatement, newVar) <- newVars envTypeR "CASin"
    -- Bind new vars with existing ExpStringEnv
    let bndVars = case nt of
          FloatingNumType TypeFloat -> bindNewVars newVar (env2TupR (Push env' (ExpString $ "uintBitsToFloat(" ++ mutAccess ++ ")")))
          FloatingNumType TypeDouble -> bindNewVars newVar (env2TupR (Push env' (ExpString $ "uint64BitsToDouble(" ++ mutAccess ++ ")")))
          _ -> error "compileAtomic: Impossible"
    -- Get old value reference of mut buffer
    let mutOld = case newVar of
          TupRpair _ (TupRsingle (VarName v)) -> v
          _ -> error "compileAtomic: Impossible"
    -- Create new vars to store compare value which equals to old value
    (compStatement, compVar) <- newVars (expType exp) "CAScomp"
    let comp = case compVar of
          TupRsingle (VarName v) -> v
          _ -> error "compileAtomic: Impossible"
    -- Create new VarNameEnv according to newVar
    let newEnv = tupR2Env newVar
    -- Compile expression to get new value for mut buffer
    (updateStatement, updateExp) <- compileStatement newEnv exp
    -- Get new value expression
    let mutNew = case updateExp of
          TupRsingle (ExpString exp') -> exp'
          _ -> error "compileAtomic: Impossible"
    -- Build atomicCAS statement
    let atomicCAS = case nt of
          FloatingNumType TypeFloat -> mutOld ++ " = uintBitsToFloat(atomicCompSwap(" ++ mutAccess ++ ", floatBitsToUint(" ++ comp ++ "), floatBitsToUint(" ++ mutNew ++ ")));\n"
          FloatingNumType TypeDouble -> mutOld ++ " = uint64BitsToDouble(atomicCompSwap(" ++ mutAccess ++ ", doubleBitsToUint64(" ++ comp ++ "), doubleBitsToUint64(" ++ mutNew ++ ")));\n"
          _ -> error "compileAtomic: Impossible"
          -- mutOld ++ " = atomicCompSwap(" ++ mutAccess ++ ", " ++ comp ++ ", " ++ mutNew ++ ");\n"
    -- Build loop statement
    let loopStatement = "do {\n" 
                          ++ bndVars
                          ++ comp ++ " = " ++ mutOld ++ ";\n" 
                          ++ updateStatement
                          ++ atomicCAS 
                          ++ "} while (" ++ mutOld ++ " != " ++ comp ++ ");\n"
    let casStatement = newStatement ++ compStatement ++ loopStatement
    return (Just casStatement, True)

compileAtomic _ _ _ = pure (Nothing, False)


-- | Try to compile exp when old mutable buffers are not used, returns Maybe statement
compileAtomicExchange
  :: VarNameEnv env
  -> ExpStringTup t 
  -- ^ Mutable buffers indexing access
  -> PreOpenExp (ArrayInstr benv) env t 
  -> State (VarCount, FuncMap, AInstrEnv benv) (Maybe String)
compileAtomicExchange env mutBuffs exp
  | True <- checkAtomicExchangeTypeSupport $ expType exp
  = do
    (exp2Statement, exp') <- compileStatement env exp
    let go :: ExpStringTup t -> ExpStringTup t -> String
        go TupRunit _ = ""
        go (TupRsingle (ExpString buff)) (TupRsingle (ExpString v)) = "atomicExchange(" ++ buff ++ ", " ++ v ++ ");\n"
        go (TupRpair l r) (TupRpair l' r') = go l l' ++ go r r'
        go _ _ = error "compileAtomicExchange: Impossible"
    return $ Just $ exp2Statement ++ go mutBuffs exp'
  | otherwise = pure Nothing

-- | Check if the type is supported by atomic exchange
checkAtomicExchangeTypeSupport :: TypeR a -> Bool
-- checkAtomicExchangeTypeSupport TupRunit = True
checkAtomicExchangeTypeSupport (TupRsingle (SingleScalarType (NumSingleType nt)))
  = case nt of
      IntegralNumType TypeInt32 -> True
      IntegralNumType TypeInt64 -> True
      IntegralNumType TypeInt -> True
      IntegralNumType TypeWord32 -> True
      IntegralNumType TypeWord64 -> True
      IntegralNumType TypeWord -> True
      FloatingNumType TypeFloat -> True
      FloatingNumType TypeDouble -> True
      _ -> False
-- checkAtomicExchangeTypeSupport (TupRpair l r) = checkAtomicExchangeTypeSupport l && checkAtomicExchangeTypeSupport r
checkAtomicExchangeTypeSupport _ = False

-- | Check if the type is supported by atomic compare-and-swap
checkAtomicCompSwapTypeSupport :: TypeR a -> Bool
-- checkAtomicCompSwapTypeSupport TupRunit = True
checkAtomicCompSwapTypeSupport (TupRsingle (SingleScalarType (NumSingleType nt)))
  = case nt of
      IntegralNumType TypeInt32 -> True
      IntegralNumType TypeInt64 -> True
      IntegralNumType TypeInt -> True
      IntegralNumType TypeWord32 -> True
      IntegralNumType TypeWord64 -> True
      IntegralNumType TypeWord -> True
      _ -> False
-- checkAtomicCompSwapTypeSupport (TupRpair l r) = checkAtomicCompSwapTypeSupport l && checkAtomicCompSwapTypeSupport r
checkAtomicCompSwapTypeSupport _ = False

-- | Check if the type can be type-casted into atomic compare-and-swap supported type
checkConvertToAtomicCASTypeSupport :: TypeR a -> Bool
-- checkConvertToAtomicCASTypeSupport TupRunit = True
checkConvertToAtomicCASTypeSupport (TupRsingle (SingleScalarType (NumSingleType nt)))
  = case nt of
      FloatingNumType TypeFloat -> True
      FloatingNumType TypeDouble -> True
      _ -> False
-- checkConvertToAtomicCASTypeSupport (TupRpair l r) = isFloats l && isFloats r
checkConvertToAtomicCASTypeSupport _ = False

-- isDoubles :: TypeR a -> Bool
-- -- isDoubles TupRunit = True
-- isDoubles (TupRsingle (SingleScalarType (NumSingleType nt)))
--   = case nt of
--       FloatingNumType TypeDouble -> True
--       _ -> False
-- -- isDoubles (TupRpair l r) = isDoubles l && isDoubles r
-- isDoubles _ = False


-- | Try to compile Atomic PrimFun, returns Maybe statement
--   Full support for atomic operations and data types:
--   - https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GL_EXT_shader_atomic_int64.txt
--   - https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_shader_atomic_float.txt
compileAtomicPrimFun :: PrimFun (a -> t') -> ExpStringEnv (((),t),t) -> OpenExp (((),t),t) benv a -> Maybe String
compileAtomicPrimFun (PrimAdd nt) env exp 
  = case nt of
      IntegralNumType TypeInt32 -> f
      IntegralNumType TypeInt64 -> f
      IntegralNumType TypeInt -> f
      IntegralNumType TypeWord32 -> f
      IntegralNumType TypeWord64 -> f
      IntegralNumType TypeWord -> f
      FloatingNumType TypeFloat -> f
      FloatingNumType TypeDouble -> f
      _ -> Nothing
    where f = case extractAtomicArgs env exp of
              Nothing -> Nothing
              Just (TupRpair (TupRsingle (ExpString x)) (TupRsingle (ExpString y))) ->
                Just $ "atomicAdd(" ++ x ++ ", " ++ y ++ ");\n"
              _ -> Nothing
compileAtomicPrimFun (PrimSub nt) env exp
  = case nt of
      IntegralNumType TypeInt32 -> f
      IntegralNumType TypeInt64 -> f
      IntegralNumType TypeInt -> f
      IntegralNumType TypeWord32 -> f
      IntegralNumType TypeWord64 -> f
      IntegralNumType TypeWord -> f
      FloatingNumType TypeFloat -> f
      FloatingNumType TypeDouble -> f
      _ -> Nothing
    where f :: Maybe String
          f = case exp of
              (Pair (Evar (Var _ ZeroIdx)) (Evar (Var _ idx@(SuccIdx ZeroIdx))))
                -> let (ExpString x) = prj' ZeroIdx env in
                    let (ExpString y) = prj' idx env in
                    Just $ "atomicAdd(" ++ x ++ ", -(" ++ y ++ "));\n"
              _ -> Nothing
compileAtomicPrimFun (PrimBAnd it) env exp
  = case it of
      TypeInt32 -> f
      TypeInt64 -> f
      TypeInt -> f
      TypeWord32 -> f
      TypeWord64 -> f
      TypeWord -> f
      _ -> Nothing
    where f = case extractAtomicArgs env exp of
              Nothing -> Nothing
              Just (TupRpair (TupRsingle (ExpString x)) (TupRsingle (ExpString y))) ->
                Just $ "atomicAnd(" ++ x ++ ", " ++ y ++ ");\n"
              _ -> Nothing
compileAtomicPrimFun (PrimBOr it) env exp
  = case it of
      TypeInt32 -> f
      TypeInt64 -> f
      TypeInt -> f
      TypeWord32 -> f
      TypeWord64 -> f
      TypeWord -> f
      _ -> Nothing
    where f = case extractAtomicArgs env exp of
              Nothing -> Nothing
              Just (TupRpair (TupRsingle (ExpString x)) (TupRsingle (ExpString y))) ->
                Just $ "atomicOr(" ++ x ++ ", " ++ y ++ ");\n"
              _ -> Nothing
compileAtomicPrimFun (PrimBXor it) env exp
  = case it of
      TypeInt32 -> f
      TypeInt64 -> f
      TypeInt -> f
      TypeWord32 -> f
      TypeWord64 -> f
      TypeWord -> f
      _ -> Nothing
    where f = case extractAtomicArgs env exp of
              Nothing -> Nothing
              Just (TupRpair (TupRsingle (ExpString x)) (TupRsingle (ExpString y))) ->
                Just $ "atomicXor(" ++ x ++ ", " ++ y ++ ");\n"
              _ -> Nothing
compileAtomicPrimFun (PrimMax ((NumSingleType nt))) env exp
  = case nt of
      IntegralNumType TypeInt32 -> f
      IntegralNumType TypeInt64 -> f
      IntegralNumType TypeInt -> f
      IntegralNumType TypeWord32 -> f
      IntegralNumType TypeWord64 -> f
      IntegralNumType TypeWord -> f
      _ -> Nothing
    where f = case extractAtomicArgs env exp of
              Nothing -> Nothing
              Just (TupRpair (TupRsingle (ExpString x)) (TupRsingle (ExpString y))) ->
                Just $ "atomicMax(" ++ x ++ ", " ++ y ++ ");\n"
              _ -> Nothing
compileAtomicPrimFun (PrimMin ((NumSingleType nt))) env exp
  = case nt of
      IntegralNumType TypeInt32 -> f
      IntegralNumType TypeInt64 -> f
      IntegralNumType TypeInt -> f
      IntegralNumType TypeWord32 -> f
      IntegralNumType TypeWord64 -> f
      IntegralNumType TypeWord -> f
      _ -> Nothing
    where f = case extractAtomicArgs env exp of
              Nothing -> Nothing
              Just (TupRpair (TupRsingle (ExpString x)) (TupRsingle (ExpString y))) ->
                Just $ "atomicMin(" ++ x ++ ", " ++ y ++ ");\n"
              _ -> Nothing
compileAtomicPrimFun _ _ _ = Nothing

-- | Try to extract Atomic args from exp, buffer access placed at the first then update value
extractAtomicArgs :: ExpStringEnv (((),t),t) -> OpenExp (((),t),t) benv a -> Maybe (ExpStringTup (t,t))
extractAtomicArgs env (Pair x y)
  | Evar (Var _ ZeroIdx) <- x
  , Evar (Var _ idx@(SuccIdx ZeroIdx)) <- y
  = Just $ TupRpair (TupRsingle (prj' ZeroIdx env)) (TupRsingle (prj' idx env))
extractAtomicArgs env (Pair x y)
  | Evar (Var _ idx@(SuccIdx ZeroIdx)) <- x
  , Evar (Var _ ZeroIdx) <- y
  = Just $ TupRpair (TupRsingle (prj' ZeroIdx env)) (TupRsingle (prj' idx env)) 
extractAtomicArgs _ _ = Nothing