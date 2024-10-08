{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Array.Accelerate.Vulkan.Compile.Expression where

import Control.Monad.State (MonadState (get, put), State)
import Data.Array.Accelerate.AST.Environment
  ( Env (Empty),
    prj',
  )
import Data.Array.Accelerate.AST.Exp (PreOpenExp (..), PreOpenFun (..), PrimConst (..), PrimFun (..), TAG, expType)
import Data.Array.Accelerate.AST.Operation (ArrayInstr (..))
import Data.Array.Accelerate.AST.Var (Var (..))
import Data.Array.Accelerate.Representation.Elt (bytesElt)
import Data.Array.Accelerate.Representation.Type
  ( TupR (..),
    TypeR,
  )
import Data.Array.Accelerate.Trafo.Exp.Substitution (rebuildNoArrayInstr)
import Data.Array.Accelerate.Type
  ( BitSizeEq,
    BoundedType (..),
    FloatingDict (..),
    FloatingType (..),
    IntegralDict (..),
    IntegralType (..),
    NumType (..),
    ScalarType (..),
    SingleType (..),
    floatingDict,
    integralDict,
  )
import Data.Array.Accelerate.Vulkan.Common (updateEnv)
import Data.Array.Accelerate.Vulkan.Compile.ArrayInstr
import Data.Array.Accelerate.Vulkan.Compile.Convert
import Data.Array.Accelerate.Vulkan.Compile.Type
import Data.Array.Accelerate.Vulkan.Compile.Index
import Data.Array.Accelerate.Vulkan.Compile.LeftHandSide
import Data.Array.Accelerate.Vulkan.Compile.Var
import Data.Map.Ordered (lookup, (|>))
import Prelude hiding (exp, init, lookup)

-- | Compile PreOpenExp structure into (Statement, TupR Expression)
compileStatement :: forall benv env t. VarNameEnv env -> PreOpenExp (ArrayInstr benv) env t -> State (VarCount, FuncMap, AInstrEnv benv) (String, ExpStringTup t) -- return (Statement, TupR Expression)

-- Compile a let-binding block into statement and expression
compileStatement env (Let lhs exp1 exp2) =
  do
    -- Compile exp1
    (e1Statement, e1Exp) <- compileStatement env exp1
    -- Update env with lhs and e1Exp
    (newEnvStatement, env') <- compileLhs env lhs e1Exp
    -- Compile exp2 using new env
    (e2Statement, e2Exp) <- compileStatement env' exp2
    return (e1Statement ++ newEnvStatement ++ e2Statement, e2Exp)

-- Compile a reference to SingleScalarType variable into expression
compileStatement env (Evar (Var (SingleScalarType _) idx)) =
  return ("", TupRsingle $ ExpString v)
  where
    (VarName v) = prj' idx env
compileStatement _ (Evar (Var (VectorScalarType _) _)) = error "compileStatement: Not implemented for VectorScalarType"

-- Compile a backend-specific foriegn function into statement and expression
compileStatement env (Foreign _ _ fallback exp) =
  do
    (expStatement, exp') <- compileStatement env exp
    -- Compile args of fallback function
    (newStatement, newVar) <- newAndBindVars (expType exp) "foreign" exp'
    -- Rebuild the fallback function into PreOpenFun
    let fbFunc = rebuildNoArrayInstr fallback
    (fbStatement, fbExp) <- compileFun Empty fbFunc newVar
    return (expStatement ++ newStatement ++ fbStatement, fbExp)

-- Compile a tuple into statement and expression
compileStatement env (Pair exp1 exp2) =
  do
    (s1, e1) <- compileStatement env exp1
    (s2, e2) <- compileStatement env exp2
    return (s1 ++ s2, TupRpair e1 e2)
compileStatement _ Nil = return ("", TupRunit)

-- Compile a SIMD vector into statement and expression
compileStatement _ (VecPack {}) = error "compileStatement: Not implemented for VecPack"
compileStatement _ (VecUnpack {}) = error "compileStatement: Not implemented for VecUnpack"

-- Compile an array slicing into statement and expression
compileStatement env (IndexSlice slix ix sh) =
  do
    (ixStatement, ixExp) <- compileStatement env ix
    (shStatement, shExp) <- compileStatement env sh
    let slIdx = compileIndexSlice slix ixExp shExp
    return (ixStatement ++ shStatement, slIdx)
compileStatement env (IndexFull slix ix sh) =
  do
    (ixStatement, ixExp) <- compileStatement env ix
    (shStatement, shExp) <- compileStatement env sh
    let fullIdx = compileIndexFull slix ixExp shExp
    return (ixStatement ++ shStatement, fullIdx)

-- Compile an index conversion into statement and expression
-- !NOTE: There is no boundry check, and it is pointless
--   Since nothing will catch and process the error or assertion thrown by Vulkan kernel.
compileStatement env (ToIndex _ sh ix) =
  do
    (shStatement, shExp) <- compileStatement env sh
    (ixStatement, ixExp) <- compileStatement env ix
    let (ExpString newIdx) = compileToIndex shExp ixExp
    return (shStatement ++ ixStatement, TupRsingle $ ExpString newIdx)
compileStatement env (FromIndex shr sh ix) =
  do
    (shStatement, shExp) <- compileStatement env sh
    (ixStatement, ixExp) <- compileStatement env ix
    (newVarStatement, newVar) <- newAndBindVars (expType ix) "fromIndex" ixExp
    case newVar of
      (TupRsingle (VarName v)) -> do
        (idxsStatement, idxs) <- compileFromIndex shr shExp (ExpString v)
        return (shStatement ++ ixStatement ++ newVarStatement ++ idxsStatement, idxs)

-- Compile a case block into statement and expression
compileStatement env (Case select cases defaultCase) =
  do
    -- Use newVars to create new return variables given return type
    (rtnVarsStatement, rtnVars) <- newVars (expType (snd $ head cases)) "caseRtn"
    -- Compile select expression
    (sStatement, sExp) <- compileStatement env select
    -- Compile cases
    let (tags, cases') = unzip cases
    cases'' <- mapM (compileStatement env) cases'
    -- Compile default case
    defaultCase' <- case defaultCase of
      Just d -> Just <$> compileStatement env d
      Nothing -> return Nothing
    return $ case sExp of
      TupRsingle sExp' -> (rtnVarsStatement ++ sStatement ++ caseBody, convertVarName2ExpString rtnVars)
        where
          caseBody = compileCase sExp' (zip tags cases'') defaultCase' rtnVars

-- Compile a condition block into statement and expression
compileStatement env (Cond c t f) =
  do
    -- Use newVars to create new return variables given return type
    (rtnVarsStatement, rtnVarsTup) <- newVars (expType t) "condRtn"
    -- Compile condition, true, and false expressions
    (cStatement, cExp) <- compileStatement env c
    (tStatement, tExp) <- compileStatement env t
    (fStatement, fExp) <- compileStatement env f
    case cExp of
      TupRsingle (ExpString cExp') -> do
        let rtnStatement =
              cStatement
                ++ rtnVarsStatement
                ++ "if (bool("
                ++ cExp'
                ++ ")) {\n"
                ++ tStatement
                ++ bindNewVars rtnVarsTup tExp
                ++ "} "
                ++ "else {\n" -- Bind return variables with true expression
                -- Bind return variables with true expression
                ++ fStatement
                ++ bindNewVars rtnVarsTup fExp
                ++ "}\n" -- Bind return variables with false expression
        return (rtnStatement, convertVarName2ExpString rtnVarsTup)

-- Compile a looping block into statement and expression
-- !NOTE: If the loop takes too long, VK may throw an error VK_ERROR_DEVICE_LOST
compileStatement env (While cond body init) =
  do
    -- Compile init value, condition, loop body
    (iStatement, iExp) <- compileStatement env init
    -- Create new mutable vars to contain init value (possibly immutable)
    (newStatement, newVar) <- newAndBindVars (expType init) "whileVars" iExp
    -- cond and body have only non-recursive let-bindings
    (cStatement, cExp) <- compileFun env cond newVar
    (bStatement, bExp) <- compileFun env body newVar
    case cExp of
      TupRsingle (ExpString cExp') -> do
        -- Update init value
        let updateStatement = updateVars newVar bExp
        let initStr = iStatement ++ newStatement ++ cStatement
        let whileStr =
              "while (bool("
                ++ cExp'
                ++ ")) {\n"
                ++ bStatement
                ++ updateStatement
                ++ cStatement
                ++ "}\n"
        return (initStr ++ whileStr, convertVarName2ExpString newVar)

-- Compile a constant value into expression
compileStatement _ (Const (SingleScalarType (NumSingleType s)) e) = return ("", TupRsingle $ ExpString (numConstToString s e))
compileStatement _ (Const (VectorScalarType _) _) = error "compileStatement: Const: Not implemented for VectorScalarType"
compileStatement _ (PrimConst primConst) = return ("", TupRsingle $ compilePrimConst primConst)

-- Compile a PrimFun into statement and expression
compileStatement env (PrimApp f exp) = compilePrimFun f env exp

-- Compile an array indexing into statement and expression
-- !NOTE: There is no boundry check, and it is pointless
-- Since nothing will catch and process the error or assertion thrown by Vulkan kernel
compileStatement env (ArrayInstr (Index gVar) exp) =
  do
    (gStatement, VarName v) <- compileArrayInstr (Index gVar)
    (expStatement, exp') <- compileStatement env exp
    case exp' of
      (TupRsingle (ExpString exp'')) -> return (gStatement ++ expStatement, TupRsingle $ ExpString (v ++ "[uint32_t(" ++ exp'' ++ ")]"))
compileStatement env (ArrayInstr (Parameter gVar) exp) =
  do
    (gStatement, VarName v) <- compileArrayInstr (Parameter gVar)
    (expStatement, exp') <- compileStatement env exp
    case exp' of
      TupRunit -> return (gStatement ++ expStatement, TupRsingle $ ExpString v)
      _ -> error "compileStatement: ArrayInstr impossible"

-- Compile a number of elements of an array given its shape into statement and expression
compileStatement env (ShapeSize _ sh) =
  do
    (shStatement, shExp) <- compileStatement env sh
    let size = compileShapeSize shExp
    return (shStatement, TupRsingle $ ExpString size)

-- Compile an undefied expression into expression
-- Put 0 for undefined value, since it is effectless
compileStatement _ (Undef (SingleScalarType (NumSingleType nt@(IntegralNumType (it :: IntegralType t)))))
  | IntegralDict <- integralDict it =
      return ("", TupRsingle $ ExpString $ numConstToString nt (0 :: t))
compileStatement _ (Undef (SingleScalarType (NumSingleType nt@(FloatingNumType (ft :: FloatingType t)))))
  | FloatingDict <- floatingDict ft =
      return ("", TupRsingle $ ExpString $ numConstToString nt (0 :: t))
compileStatement _ (Undef (VectorScalarType _)) = error "compileStatement: Undef: Not implemented for VectorScalarType"

-- Compile a bits reinterpretation into expression
compileStatement env (Coerce (SingleScalarType (NumSingleType nt1)) (SingleScalarType (NumSingleType nt2)) exp1) =
  do
    (expStatement, exp1') <- compileStatement env exp1
    case exp1' of
      (TupRsingle (ExpString exp1'')) -> do
        let newType = compileSSCoerce nt1 nt2
        return (expStatement, TupRsingle $ ExpString $ newType ++ "(" ++ exp1'' ++ ")")
      _ -> error $ "compileStatement: Coerce impossible: ExpStringTup " ++ show exp1'
compileStatement _ (Coerce (VectorScalarType _) _ _) = error "compileStatement: Coerce: Not implemented for VectorScalarType"
compileStatement _ (Coerce _ (VectorScalarType _) _) = error "compileStatement: Coerce: Not implemented for VectorScalarType"

-- | Compile Case into Statement, given select expression, cases, default case, and return variables
compileCase :: ExpString TAG -> [(TAG, (String, ExpStringTup t))] -> Maybe (String, ExpStringTup t) -> VarNameTup t -> String
compileCase (ExpString sExp) cases mbDefault rtnVars =
  case mbDefault of
    Just (dStatement, dExp) -> caseStr
      where
        caseDefault =
          "default:\n"
            ++ dStatement
            ++ bindNewVars rtnVars dExp
            ++ "break;\n"
        caseStr = "switch (int32_t(" ++ sExp ++ ")) {\n" ++ caseBody ++ caseDefault ++ "}\n"
    Nothing -> caseStr -- No default case
      where
        caseStr = "switch (int32_t(" ++ sExp ++ ")) {\n" ++ caseBody ++ "}\n"
  where
    -- Compile case blocks
    caseBody =
      foldl
        ( \acc (n, (s, exp)) ->
            acc
              ++ "case "
              ++ show n
              ++ ":\n"
              ++ s
              ++ bindNewVars rtnVars exp
              ++ "break;\n"
        )
        ""
        cases

-- | Compile PreOpenFun into (Statement, Function Name)
compileFun :: forall benv env a b. VarNameEnv env -> PreOpenFun (ArrayInstr benv) env (a -> b) -> VarNameTup a -> State (VarCount, FuncMap, AInstrEnv benv) (String, ExpStringTup b)
compileFun env (Lam lhs (Body body)) args = do
  let env' = updateEnv env lhs args
  (bodyStatement, bodyExp) <- compileStatement env' body
  return (bodyStatement, bodyExp)
compileFun _ _ _ = error "compileFun: Impossible"

-- | Compile PrimConst into expression
compilePrimConst :: PrimConst a -> ExpString a
compilePrimConst (PrimMinBound (IntegralBoundedType (it :: IntegralType t)))
  | IntegralDict <- integralDict it =
      ExpString $ numConstToString (IntegralNumType it) (minBound :: t)
compilePrimConst (PrimMaxBound (IntegralBoundedType (it :: IntegralType t)))
  | IntegralDict <- integralDict it =
      ExpString $ numConstToString (IntegralNumType it) (maxBound :: t)
compilePrimConst (PrimPi (ft :: FloatingType t))
  | FloatingDict <- floatingDict ft =
      ExpString $ numConstToString (FloatingNumType ft) (pi :: t)

-- compilePrimConst _ = error "compilePrimConst: Impossible"

-- | Compile SingleScalarType bits reinterpretation into expression
compileSSCoerce :: (BitSizeEq a b) => NumType a -> NumType b -> String
compileSSCoerce (IntegralNumType _) (IntegralNumType b) = integralTypeToString b
compileSSCoerce (FloatingNumType _) (FloatingNumType b) = floatingTypeToString b
compileSSCoerce (FloatingNumType a) (IntegralNumType b) =
  case (a, b) of
    (TypeHalf, TypeInt16) -> "halfBitsToInt16"
    (TypeHalf, TypeWord16) -> "halfBitsToUint16"
    (TypeFloat, TypeInt32) -> "floatBitsToInt"
    (TypeFloat, TypeWord32) -> "floatBitsToUint"
    (TypeDouble, TypeInt64) -> "doubleBitsToInt64"
    (TypeDouble, TypeWord64) -> "doubleBitsToUint64"
    (a', b') -> error $ "compileSSCoerce: Type " ++ show a' ++ " -> " ++ show b' ++ " conversion impossible"
compileSSCoerce (IntegralNumType a) (FloatingNumType b) =
  case (a, b) of
    (TypeInt16, TypeHalf) -> "int16BitsToHalf"
    (TypeWord16, TypeHalf) -> "uint16BitsToHalf"
    (TypeInt32, TypeFloat) -> "intBitsToFloat"
    (TypeWord32, TypeFloat) -> "uintBitsToFloat"
    (TypeInt64, TypeDouble) -> "int64BitsToDouble"
    (TypeWord64, TypeDouble) -> "uint64BitsToDouble"
    (a', b') -> error $ "compileSSCoerce: " ++ show a' ++ " -> " ++ show b' ++ " conversion impossible"

-- | Compile PrimFun into (Statement, TupR Expression)
compilePrimFun :: PrimFun (a -> t) -> VarNameEnv env -> PreOpenExp (ArrayInstr benv) env a -> State (VarCount, FuncMap, AInstrEnv benv) (String, ExpStringTup t)
-- Compile operators from Num
compilePrimFun (PrimAdd _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("(" ++ exp1 ++ ") + (" ++ exp2 ++ ")"))
      _ -> error "compilePrimFun: PrimAdd: Impossible"
compilePrimFun (PrimSub _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("(" ++ exp1 ++ ") - (" ++ exp2 ++ ")"))
      _ -> error "compilePrimFun: PrimSub: Impossible"
compilePrimFun (PrimMul _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("(" ++ exp1 ++ ") * (" ++ exp2 ++ ")"))
      _ -> error "compilePrimFun: PrimMul: Impossible"
compilePrimFun (PrimNeg _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString ("-" ++ "(" ++ exp'' ++ ")"))
      _ -> error "compilePrimFun: PrimNeg: Impossible"
compilePrimFun (PrimAbs _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString ("abs(" ++ exp'' ++ ")"))
      _ -> error "compilePrimFun: PrimAbs: Impossible"
compilePrimFun (PrimSig _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString ("sign(" ++ exp'' ++ ")"))
      _ -> error "compilePrimFun: PrimSig: Impossible"

-- Compile operators from Integral
compilePrimFun (PrimQuot it) env exp =
  do
    (quotStatement, quotExp, _, _) <- compilePrimQuot env it exp
    return (quotStatement, TupRsingle quotExp)
compilePrimFun (PrimRem it) env exp =
  do
    (quotStatement, ExpString quotExp, ExpString x, ExpString y) <- compilePrimQuot env it exp
    let remStr = x ++ " - " ++ y ++ " * (" ++ quotExp ++ ")"
    return (quotStatement, TupRsingle $ ExpString remStr)
compilePrimFun (PrimQuotRem it) env exp =
  do
    (quotStatement, ExpString quotExp, ExpString x, ExpString y) <- compilePrimQuot env it exp
    let varTp = TupRsingle (SingleScalarType (NumSingleType (IntegralNumType it)))
    expQuot <- newAndBindVars varTp "quotExp" (TupRsingle (ExpString quotExp))
    case expQuot of
      (quotVarStatement, TupRsingle (VarName quotVar)) -> do
        let remStr = x ++ " - " ++ y ++ " * " ++ quotVar
        return (quotStatement ++ quotVarStatement, TupRpair (TupRsingle $ ExpString quotVar) (TupRsingle $ ExpString remStr))
      _ -> error "compilePrimFun: PrimQuotRem: Impossible"
compilePrimFun (PrimIDiv _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("(" ++ exp1 ++ ") / (" ++ exp2 ++ ")"))
      _ -> error "compilePrimFun: PrimIDiv: Impossible"
compilePrimFun (PrimMod _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("(" ++ exp1 ++ ") % (" ++ exp2 ++ ")"))
      _ -> error "compilePrimFun: PrimMod: Impossible"
compilePrimFun (PrimDivMod _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return
          ( expStatement,
            TupRpair
              (TupRsingle $ ExpString ("(" ++ exp1 ++ ") / (" ++ exp2 ++ ")"))
              (TupRsingle $ ExpString ("(" ++ exp1 ++ ") % (" ++ exp2 ++ ")"))
          )
      _ -> error "compilePrimFun: PrimDivMod: Impossible"

-- Compile operators from Bits & FiniteBits
compilePrimFun (PrimBAnd _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("(" ++ exp1 ++ ") & (" ++ exp2 ++ ")"))
      _ -> error "compilePrimFun: PrimBAnd: Impossible"
compilePrimFun (PrimBOr _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("(" ++ exp1 ++ ") | (" ++ exp2 ++ ")"))
      _ -> error "compilePrimFun: PrimBOr: Impossible"
compilePrimFun (PrimBXor _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("(" ++ exp1 ++ ") ^ (" ++ exp2 ++ ")"))
      _ -> error "compilePrimFun: PrimBXor: Impossible"
compilePrimFun (PrimBNot _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString ("~(" ++ exp'' ++ ")"))
      _ -> error "compilePrimFun: PrimBNot: Impossible"
-- According to the GLSLangSpec 4.60, the following op:
-- The result is undefined if the right operand is negative, or greater than or
-- equal to the number of bits in the left expression’s base type.
compilePrimFun (PrimBShiftL _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("(" ++ exp1 ++ ") << (" ++ exp2 ++ ")"))
      _ -> error "compilePrimFun: PrimBShiftL: Impossible"
-- According to the GLSLangSpec 4.60, the following op:
-- The result is undefined if the right operand is negative, or greater than or
-- equal to the number of bits in the left expression’s base type.
compilePrimFun (PrimBShiftR _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("(" ++ exp1 ++ ") >> (" ++ exp2 ++ ")"))
      _ -> error "compilePrimFun: PrimBShiftR: Impossible"
-- According to the GLSLangSpec 4.60, the following op:
-- The result is undefined if the right operand is negative, or greater than or
-- equal to the number of bits in the left expression’s base type.
compilePrimFun (PrimBRotateL it) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString v1)) (TupRsingle (ExpString v2))) -> do
        let v1Tp = SingleScalarType (NumSingleType (IntegralNumType it))
        -- https://stackoverflow.com/questions/10134805/bitwise-rotate-left-function
        let rtnStr = "(" ++ v1 ++ " << " ++ v2 ++ ") | (" ++ v1 ++ " >> (" ++ show (8 * bytesElt (TupRsingle v1Tp)) ++ " - " ++ v2 ++ ")) & ~(-1 << " ++ v2 ++ ")"
        return (expStatement, TupRsingle $ ExpString rtnStr)
      _ -> error "compilePrimFun: PrimBRotateL: Impossible"
-- According to the GLSLangSpec 4.60, the following op:
-- The result is undefined if the right operand is negative, or greater than or
-- equal to the number of bits in the left expression’s base type.
compilePrimFun (PrimBRotateR it) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString v1)) (TupRsingle (ExpString v2))) -> do
        let v1Tp = SingleScalarType (NumSingleType (IntegralNumType it))
        -- https://stackoverflow.com/questions/10134805/bitwise-rotate-left-function
        let rtnStr = "(" ++ v1 ++ " >> " ++ v2 ++ ") | (" ++ v1 ++ " << (" ++ show (8 * bytesElt (TupRsingle v1Tp)) ++ " - " ++ v2 ++ ")) & ~(-1 >> " ++ v2 ++ ")"
        return (expStatement, TupRsingle $ ExpString rtnStr)
      _ -> error "compilePrimFun: PrimBRotateR: Impossible"
compilePrimFun (PrimPopCount _) env exp =
  do
    (rtnStatement, rtnExp) <- compilePrimPopCount env (expType exp) exp
    return (rtnStatement, TupRsingle rtnExp)
compilePrimFun (PrimCountLeadingZeros _) env exp =
  do
    (rtnStatement, rtnExp) <- compilePrimClz env (expType exp) exp
    return (rtnStatement, TupRsingle rtnExp)
compilePrimFun (PrimCountTrailingZeros _) env exp =
  do
    (rtnStatement, rtnExp) <- compilePrimCtz env (expType exp) exp
    return (rtnStatement, TupRsingle rtnExp)

-- Compile operators from Fractional and Floating
compilePrimFun (PrimFDiv ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("(" ++ exp1 ++ ") / (" ++ exp2 ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("(" ++ exp1 ++ ") / (" ++ exp2 ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimFDiv: Double not supported"
      _ -> error "compilePrimFun: PrimFDiv: Impossible"
compilePrimFun (PrimRecip ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString (numConstToString (FloatingNumType ft) 1 ++ " / (" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString (numConstToString (FloatingNumType ft) 1 ++ " / (" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimRecip: Double not supported"
      _ -> error "compilePrimFun: PrimRecip: Impossible"
compilePrimFun (PrimSin ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("sin(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("sin(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimSin: Double not supported"
      _ -> error "compilePrimFun: PrimSin: Impossible"
compilePrimFun (PrimCos ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("cos(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("cos(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimCos: Double not supported"
      _ -> error "compilePrimFun: PrimCos: Impossible"
compilePrimFun (PrimTan ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("tan(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("tan(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimTan: Double not supported"
      _ -> error "compilePrimFun: PrimTan: Impossible"
compilePrimFun (PrimAsin ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("asin(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("asin(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimAsin: Double not supported"
      _ -> error "compilePrimFun: PrimAsin: Impossible"
compilePrimFun (PrimAcos ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("acos(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("acos(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimAcos: Double not supported"
      _ -> error "compilePrimFun: PrimAcos: Impossible"
compilePrimFun (PrimAtan ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("atan(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("atan(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimAtan: Double not supported"
      _ -> error "compilePrimFun: PrimAtan: Impossible"
compilePrimFun (PrimSinh ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("sinh(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("sinh(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimSinh: Double not supported"
      _ -> error "compilePrimFun: PrimSinh: Impossible"
compilePrimFun (PrimCosh ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("cosh(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("cosh(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimCosh: Double not supported"
      _ -> error "compilePrimFun: PrimCosh: Impossible"
compilePrimFun (PrimTanh ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("tanh(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("tanh(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimTanh: Double not supported"
      _ -> error "compilePrimFun: PrimTanh: Impossible"
compilePrimFun (PrimAsinh ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("asinh(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("asinh(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimAsinh: Double not supported"
      _ -> error "compilePrimFun: PrimAsinh: Impossible"
compilePrimFun (PrimAcosh ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("acosh(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("acosh(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimAcosh: Double not supported"
      _ -> error "compilePrimFun: PrimAcosh: Impossible"
compilePrimFun (PrimAtanh ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("atanh(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("atanh(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimAtanh: Double not supported"
      _ -> error "compilePrimFun: PrimAtanh: Impossible"
compilePrimFun (PrimExpFloating ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("exp(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("exp(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimExpFloating: Double not supported"
      _ -> error "compilePrimFun: PrimExpFloating: Impossible"
compilePrimFun (PrimSqrt _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString ("sqrt(" ++ exp'' ++ ")"))
      _ -> error "compilePrimFun: PrimSqrt: Impossible"
compilePrimFun (PrimLog it) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> case it of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("log(" ++ exp'' ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("log(" ++ exp'' ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimLog: Double not supported"
      _ -> error "compilePrimFun: PrimLog: Impossible"
compilePrimFun (PrimFPow ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("pow(" ++ exp1 ++ ", " ++ exp2 ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("pow(" ++ exp1 ++ ", " ++ exp2 ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimFPow: Double not supported"
      _ -> error "compilePrimFun: PrimFPow: Impossible"
compilePrimFun (PrimLogBase ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("log(" ++ exp2 ++ ") / log(" ++ exp1 ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("log(" ++ exp2 ++ ") / log(" ++ exp1 ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimLogBase: Double not supported"
      _ -> error "compilePrimFun: PrimLogBase: Impossible"

-- Compile operators from RealFrac
compilePrimFun (PrimTruncate _ it) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString $ integralTypeToString it ++ ("(trunc(" ++ exp'' ++ "))"))
      _ -> error "compilePrimFun: PrimTruncate: Impossible"
compilePrimFun (PrimRound _ it) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString $ integralTypeToString it ++ ("(round(" ++ exp'' ++ "))"))
      _ -> error "compilePrimFun: PrimRound: Impossible"
compilePrimFun (PrimFloor _ it) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString $ integralTypeToString it ++ ("(floor(" ++ exp'' ++ "))"))
      _ -> error "compilePrimFun: PrimFloor: Impossible"
compilePrimFun (PrimCeiling _ it) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString $ integralTypeToString it ++ ("(ceil(" ++ exp'' ++ "))"))
      _ -> error "compilePrimFun: PrimCeiling: Impossible"

-- Compile operators from RealFloat
compilePrimFun (PrimAtan2 ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) -> case ft of
        TypeHalf -> return (expStatement, TupRsingle $ ExpString ("atan(" ++ exp1 ++ ", " ++ exp2 ++ ")"))
        TypeFloat -> return (expStatement, TupRsingle $ ExpString ("atan(" ++ exp1 ++ ", " ++ exp2 ++ ")"))
        TypeDouble -> error "compilePrimFun: PrimAtan2: Double not supported"
      _ -> error "compilePrimFun: PrimAtan2: Impossible"
compilePrimFun (PrimIsNaN _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString ("uint8_t(isnan(" ++ exp'' ++ "))"))
      _ -> error "compilePrimFun: PrimIsNaN: Impossible"
compilePrimFun (PrimIsInfinite _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString ("uint8_t(isinf(" ++ exp'' ++ "))"))
      _ -> error "compilePrimFun: PrimIsInfinite: Impossible"

-- Compile relational and equality operators
compilePrimFun (PrimLt _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("uint8_t((" ++ exp1 ++ ") < (" ++ exp2 ++ "))"))
      _ -> error "compilePrimFun: PrimLt: Impossible"
compilePrimFun (PrimGt _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("uint8_t((" ++ exp1 ++ ") > (" ++ exp2 ++ "))"))
      _ -> error "compilePrimFun: PrimGt: Impossible"
compilePrimFun (PrimLtEq _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("uint8_t((" ++ exp1 ++ ") <= (" ++ exp2 ++ "))"))
      _ -> error "compilePrimFun: PrimLtEq: Impossible"
compilePrimFun (PrimGtEq _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("uint8_t((" ++ exp1 ++ ") >= (" ++ exp2 ++ "))"))
      _ -> error "compilePrimFun: PrimGtEq: Impossible"
compilePrimFun (PrimEq _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("uint8_t((" ++ exp1 ++ ") == (" ++ exp2 ++ "))"))
      _ -> error "compilePrimFun: PrimEq: Impossible"
compilePrimFun (PrimNEq _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("uint8_t((" ++ exp1 ++ ") != (" ++ exp2 ++ "))"))
      _ -> error "compilePrimFun: PrimNEq: Impossible"
compilePrimFun (PrimMax _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("max(" ++ exp1 ++ ", " ++ exp2 ++ ")"))
      _ -> error "compilePrimFun: PrimMax: Impossible"
compilePrimFun (PrimMin _) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("min(" ++ exp1 ++ ", " ++ exp2 ++ ")"))
      _ -> error "compilePrimFun: PrimMin: Impossible"

-- Compile logical operators
compilePrimFun PrimLAnd env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("uint8_t(bool(" ++ exp1 ++ ") && bool(" ++ exp2 ++ "))"))
      _ -> error "compilePrimFun: PrimLAnd: Impossible"
compilePrimFun PrimLOr env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRpair (TupRsingle (ExpString exp1)) (TupRsingle (ExpString exp2))) ->
        return (expStatement, TupRsingle $ ExpString ("uint8_t(bool((" ++ exp1 ++ ") || bool(" ++ exp2 ++ "))"))
      _ -> error "compilePrimFun: PrimLOr: Impossible"
compilePrimFun PrimLNot env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString ("uint8_t(!bool(" ++ exp'' ++ "))"))

-- Compile general conversion between types
compilePrimFun (PrimFromIntegral _ nt) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString $ numTypeToString nt ++ ("(" ++ exp'' ++ ")"))
      _ -> error "compilePrimFun: PrimFromIntegral: Impossible"
compilePrimFun (PrimToFloating _ ft) env exp =
  do
    exp' <- compileStatement env exp
    case exp' of
      (expStatement, TupRsingle (ExpString exp'')) -> return (expStatement, TupRsingle $ ExpString $ floatingTypeToString ft ++ ("(" ++ exp'' ++ ")"))
      _ -> error "compilePrimFun: PrimToFloating: Impossible"

-- | Compile PrimQuot into (Statement, Result, Dividend, Divisor)
--    This is a polymorphic function, so that it is decided not to form as a GLSL function
compilePrimQuot :: VarNameEnv env -> IntegralType a -> PreOpenExp (ArrayInstr benv) env (a, a) -> State (VarCount, FuncMap, AInstrEnv benv) (String, ExpString t, ExpString t, ExpString t)
compilePrimQuot env it exp =
  do
    (expStatement, exp') <- compileStatement env exp
    (newVarStatement, newVar) <- newAndBindVars (expType exp) "quot" exp'
    case newVar of
      TupRpair (TupRsingle (VarName v1)) (TupRsingle (VarName v2)) -> do
        (singVarStatement, singVar) <-
          newAndBindVars
            (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType it))))
            "quotSign"
            (TupRsingle $ ExpString $ v1 ++ " * " ++ v2 ++ " < 0 ? -1 : 1")
        let signName = case singVar of
              TupRsingle (VarName v) -> v
              _ -> error "compilePrimQuot: Impossible"
        let varStr = signName ++ " * (abs(" ++ v1 ++ ") / abs(" ++ v2 ++ "))"
        return (expStatement ++ newVarStatement ++ singVarStatement, ExpString varStr, ExpString v1, ExpString v2)
      _ -> error "compilePrimQuot: Impossible"

-- | Compile PrimPopCount into (Statement, TupR Expression)
compilePrimPopCount :: VarNameEnv env -> TypeR a -> PreOpenExp (ArrayInstr benv) env a -> State (VarCount, FuncMap, AInstrEnv benv) (String, ExpString t)
compilePrimPopCount env (TupRsingle st) exp
  | SingleScalarType (NumSingleType (IntegralNumType it)) <- st =
      do
        (expStatement, exp') <- compileStatement env exp
        case exp' of
          TupRsingle (ExpString exp'') -> do
            case it of
              -- Unsigned conversion in GLSL is bit-presentation preserving
              -- https://stackoverflow.com/questions/50146411/can-i-reinterpret-cast-in-glsl
              -- binCount only supports 32-bit integer
              TypeInt8 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(binCount(uint32_t(uint8_t(" ++ exp'' ++ ")))")
              TypeInt16 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(binCount(uint32_t(uint16_t(" ++ exp'' ++ ")))")
              TypeInt32 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(binCount(uint32_t(" ++ exp'' ++ ")))")
              TypeInt64 -> do
                (bcStatement, bcExp) <- popCount64 (ExpString $ "uint64_t(" ++ exp'' ++ ")")
                return (expStatement ++ bcStatement, bcExp)
              TypeInt -> do
                (bcStatement, bcExp) <- popCount64 (ExpString $ "uint64_t(" ++ exp'' ++ ")")
                return (expStatement ++ bcStatement, bcExp)
              TypeWord8 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(binCount(uint32_t(" ++ exp'' ++ ")))")
              TypeWord16 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(binCount(uint32_t(" ++ exp'' ++ ")))")
              TypeWord32 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(binCount(" ++ exp'' ++ "))")
              TypeWord64 -> do
                (bcStatement, bcExp) <- popCount64 (ExpString exp'')
                return (expStatement ++ bcStatement, bcExp)
              TypeWord -> do
                (bcStatement, bcExp) <- popCount64 (ExpString exp'')
                return (expStatement ++ bcStatement, bcExp)
          _ -> error "compilePrimPopCount: IntegralNumType: Impossible"
compilePrimPopCount env (TupRsingle st) exp
  | SingleScalarType (NumSingleType (FloatingNumType ft)) <- st =
      do
        (expStatement, exp') <- compileStatement env exp
        case exp' of
          TupRsingle (ExpString exp'') -> do
            case ft of
              TypeHalf -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(binCount(uint32_t(float16BitsToUint16(" ++ exp'' ++ "))))")
              TypeFloat -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(binCount(floatBitsToUint(" ++ exp'' ++ ")))")
              TypeDouble -> do
                (bcStatement, bcExp) <- popCount64 (ExpString $ "doubleBitsToUint64(" ++ exp'' ++ ")")
                return (expStatement ++ bcStatement, bcExp)
          _ -> error "compilePrimPopCount: FloatingNumType: Impossible"
compilePrimPopCount _ _ _ = error "compilePrimPopCount: Impossible"

-- | GLSL expression of calling pc64
popCount64 :: ExpString a -> State (VarCount, FuncMap, AInstrEnv benv) (String, ExpString t)
popCount64 (ExpString exp64) = do
  popCount64Deps
  -- Conver the result back to Int
  return ("", ExpString $ integralTypeToString TypeInt ++ "(pc64(" ++ exp64 ++ "))")

-- | Add dependencies for calling pc64
popCount64Deps :: State (VarCount, FuncMap, AInstrEnv benv) ()
popCount64Deps = do
  (varCount, funcMap, aInstrMap) <- get
  case lookup "pc64" funcMap of
    Nothing -> do
      let funcMap' = funcMap |> ("pc64", popCount64GLSL)
      put (varCount, funcMap', aInstrMap)
    Just _ -> return ()

-- | GLSL function of pop count (bit count) for 64-bit unsigned integer
popCount64GLSL :: String
popCount64GLSL =
  -- https://stackoverflow.com/questions/2709430/count-number-of-bits-in-a-64-bit-long-big-integer
  "int pc64(uint64_t x) {\n"
    ++ "  x = x - ((x >> 1) & 0x5555555555555555UL);\n"
    ++ "  x = (x & 0x3333333333333333UL) + ((x >> 2) & 0x3333333333333333UL);\n"
    ++ "  x = (((x + (x >> 4)) & 0xF0F0F0F0F0F0F0FUL) * 0x101010101010101UL) >> 56;\n"
    ++ "  return int(x);\n"
    ++ "}\n"

-- | Compile PrimCountLeadingZeros into (Statement, TupR Expression)
compilePrimClz :: VarNameEnv env -> TypeR a -> PreOpenExp (ArrayInstr benv) env a -> State (VarCount, FuncMap, AInstrEnv benv) (String, ExpString t)
compilePrimClz env (TupRsingle st) exp
  | SingleScalarType (NumSingleType (IntegralNumType it)) <- st =
      do
        (expStatement, exp') <- compileStatement env exp
        case exp' of
          TupRsingle (ExpString exp'') -> do
            case it of
              -- findMSB only supports 32-bit integer
              TypeInt8 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(" ++ show (8 * bytesElt (TupRsingle st) - 1) ++ " - findMSB(uint32_t(uint8_t(" ++ exp'' ++ "))))")
              TypeInt16 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(" ++ show (8 * bytesElt (TupRsingle st) - 1) ++ " - findMSB(uint32_t(uint16_t(" ++ exp'' ++ "))))")
              TypeInt32 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(" ++ show (8 * bytesElt (TupRsingle st) - 1) ++ " - findMSB(uint32_t(" ++ exp'' ++ ")))")
              TypeInt64 -> do
                (clzStatement, clzExp) <- countLeadingZeros64 (ExpString $ "uint64_t(" ++ exp'' ++ ")")
                return (expStatement ++ clzStatement, clzExp)
              TypeInt -> do
                (clzStatement, clzExp) <- countLeadingZeros64 (ExpString $ "uint64_t(" ++ exp'' ++ ")")
                return (expStatement ++ clzStatement, clzExp)
              TypeWord8 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(" ++ show (8 * bytesElt (TupRsingle st) - 1) ++ " - findMSB(uint32_t(" ++ exp'' ++ ")))")
              TypeWord16 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(" ++ show (8 * bytesElt (TupRsingle st) - 1) ++ " - findMSB(uint32_t(" ++ exp'' ++ ")))")
              TypeWord32 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(" ++ show (8 * bytesElt (TupRsingle st) - 1) ++ " - findMSB(" ++ exp'' ++ "))")
              TypeWord64 -> do
                (clzStatement, clzExp) <- countLeadingZeros64 (ExpString exp'')
                return (expStatement ++ clzStatement, clzExp)
              TypeWord -> do
                (clzStatement, clzExp) <- countLeadingZeros64 (ExpString exp'')
                return (expStatement ++ clzStatement, clzExp)
          _ -> error "compilePrimClz: IntegralNumType: Impossible"
compilePrimClz env (TupRsingle st) exp
  | SingleScalarType (NumSingleType (FloatingNumType ft)) <- st =
      do
        (expStatement, exp') <- compileStatement env exp
        case exp' of
          TupRsingle (ExpString exp'') -> do
            case ft of
              TypeHalf -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(" ++ show (8 * bytesElt (TupRsingle st) - 1) ++ " - findMSB(uint32_t(float16BitsToUint16(" ++ exp'' ++ "))))")
              TypeFloat -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(" ++ show (8 * bytesElt (TupRsingle st) - 1) ++ " - findMSB(floatBitsToUint(" ++ exp'' ++ ")))")
              TypeDouble -> do
                (clzStatement, clzExp) <- countLeadingZeros64 (ExpString $ "doubleBitsToUint64(" ++ exp'' ++ ")")
                return (expStatement ++ clzStatement, clzExp)
          _ -> error "compilePrimClz: FloatingNumType: Impossible"
compilePrimClz _ _ _ = error "compilePrimClz: Impossible"

-- | GLSL expression of calling clz64
countLeadingZeros64 :: ExpString a -> State (VarCount, FuncMap, AInstrEnv benv) (String, ExpString t)
countLeadingZeros64 (ExpString exp64) = do
  countLeadingZeros64Deps
  -- Conver the result back to Int
  return ("", ExpString $ integralTypeToString TypeInt ++ "(clz64(" ++ exp64 ++ "))")

-- | Add dependencies for calling clz64
countLeadingZeros64Deps :: State (VarCount, FuncMap, AInstrEnv benv) ()
countLeadingZeros64Deps = do
  popCount64Deps
  (varCount, funcMap, aInstrMap) <- get
  case lookup "clz64" funcMap of
    Nothing -> do
      let funcMap' = funcMap |> ("clz64", countLeadingZeros64GLSL)
      put (varCount, funcMap', aInstrMap)
    Just _ -> return ()

-- | GLSL function of count leading zeros for 64-bit unsigned integer
countLeadingZeros64GLSL :: String
countLeadingZeros64GLSL =
  -- Method based on Hacker's Delight
  -- https://stackoverflow.com/questions/23856596/how-to-count-leading-zeros-in-a-32-bit-unsigned-integer
  "int clz64(uint64_t x) {\n"
    ++ "  x = x | (x >> 1);\n"
    ++ "  x = x | (x >> 2);\n"
    ++ "  x = x | (x >> 4);\n"
    ++ "  x = x | (x >> 8);\n"
    ++ "  x = x | (x >> 16);\n"
    ++ "  x = x | (x >> 32);\n"
    ++ "  x = ~x;\n"
    ++ "  return pc64(x);\n"
    ++ "}\n"

-- | Compile PrimCountTrailingZeros into (Statement, TupR Expression)
compilePrimCtz :: VarNameEnv env -> TypeR a -> PreOpenExp (ArrayInstr benv) env a -> State (VarCount, FuncMap, AInstrEnv benv) (String, ExpString t)
compilePrimCtz env (TupRsingle st) exp
  | SingleScalarType (NumSingleType (IntegralNumType it)) <- st =
      do
        (expStatement, exp') <- compileStatement env exp
        case exp' of
          TupRsingle (ExpString exp'') -> do
            case it of
              TypeInt8 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(findLSB(uint32_t(uint8_t(" ++ exp'' ++ "))))")
              TypeInt16 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(findLSB(uint32_t(uint16_t(" ++ exp'' ++ "))))")
              TypeInt32 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(findLSB(uint32_t(" ++ exp'' ++ ")))")
              TypeInt64 -> do
                (ctzStatement, ctzExp) <- countTrailingZeros64 (ExpString $ "uint64_t(" ++ exp'' ++ ")")
                return (expStatement ++ ctzStatement, ctzExp)
              TypeInt -> do
                (ctzStatement, ctzExp) <- countTrailingZeros64 (ExpString $ "uint64_t(" ++ exp'' ++ ")")
                return (expStatement ++ ctzStatement, ctzExp)
              TypeWord8 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(findLSB(uint32_t(" ++ exp'' ++ ")))")
              TypeWord16 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(findLSB(uint32_t(" ++ exp'' ++ ")))")
              TypeWord32 -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(findLSB(" ++ exp'' ++ "))")
              TypeWord64 -> do
                (ctzStatement, ctzExp) <- countTrailingZeros64 (ExpString exp'')
                return (expStatement ++ ctzStatement, ctzExp)
              TypeWord -> do
                (ctzStatement, ctzExp) <- countTrailingZeros64 (ExpString exp'')
                return (expStatement ++ ctzStatement, ctzExp)
          _ -> error "compilePrimCtz: IntegralNumType: Impossible"
compilePrimCtz env (TupRsingle st) exp
  | SingleScalarType (NumSingleType (FloatingNumType ft)) <- st =
      do
        (expStatement, exp') <- compileStatement env exp
        case exp' of
          TupRsingle (ExpString exp'') -> do
            case ft of
              TypeHalf -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(findLSB(uint32_t(float16BitsToUint16(" ++ exp'' ++ "))))")
              TypeFloat -> return (expStatement, ExpString $ integralTypeToString TypeInt ++ "(findLSB(floatBitsToUint(" ++ exp'' ++ ")))")
              TypeDouble -> do
                (ctzStatement, ctzExp) <- countTrailingZeros64 (ExpString $ "doubleBitsToUint64(" ++ exp'' ++ ")")
                return (expStatement ++ ctzStatement, ctzExp)
          _ -> error "compilePrimCtz: FloatingNumType: Impossible"
compilePrimCtz _ _ _ = error "compilePrimCtz: Impossible"

-- | GLSL expression of calling ctz64
countTrailingZeros64 :: ExpString a -> State (VarCount, FuncMap, AInstrEnv benv) (String, ExpString t)
countTrailingZeros64 (ExpString exp64) = do
  countTrailingZeros64Deps
  -- Conver the result back to Int
  return ("", ExpString $ integralTypeToString TypeInt ++ "(ctz64(" ++ exp64++ "))")

-- | Add dependencies for calling ctz64
countTrailingZeros64Deps :: State (VarCount, FuncMap, AInstrEnv benv) ()
countTrailingZeros64Deps = do
  popCount64Deps
  (varCount, funcMap, aInstrMap) <- get
  case lookup "ctz64" funcMap of
    Nothing -> do
      let funcMap' = funcMap |> ("ctz64", countTrailingZeros64GLSL)
      put (varCount, funcMap', aInstrMap)
    Just _ -> return ()

-- | GLSL function of count trailing zeros for 64-bit unsigned integer
countTrailingZeros64GLSL :: String
countTrailingZeros64GLSL =
  -- Method from Hacker's Delight
  -- https://www.oreilly.com/library/view/hackers-delight/0201914654/0201914654_ch05lev1sec4.html
  "int ctz64(uint64_t x) {\n"
    ++ "  x = ~x & (x - 1);\n"
    ++ "  return pc64(x);\n"
    ++ "}\n"
