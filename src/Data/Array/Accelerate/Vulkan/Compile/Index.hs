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

module Data.Array.Accelerate.Vulkan.Compile.Index where

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


-- | Compile array index into linearized index, give an GLSL expression
--    The shape of the array is given by the first argument
--    The index of the array is given by the second argument
compileToIndex :: ExpStringTup sh -> ExpStringTup sh -> ExpString Int
compileToIndex TupRunit TupRunit = ExpString "0"
compileToIndex (TupRpair shs (TupRsingle (ExpString sh))) (TupRpair ixs (TupRsingle (ExpString ix))) =
  ExpString $ "(" ++ lowerIdx ++ ")" ++ " * " ++ sh ++ " + " ++ ix
  where (ExpString lowerIdx) = compileToIndex shs ixs
compileToIndex _ _ = error "compileToIndex: impossible"

-- | Compile linearized index into array index, give an GLSL expression
compileFromIndex :: ShapeR sh -> ExpStringTup sh -> ExpString Int -> State (VarCount, FuncMap, AInstrEnv benv) (String, ExpStringTup sh)
compileFromIndex ShapeRz TupRunit _ = return ("", TupRunit)
compileFromIndex (ShapeRsnoc shr) (TupRpair shs (TupRsingle (ExpString sh))) (ExpString ix)
  = do
    (newIdxStatement, newIdx) <- newAndBindVars (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt)))) "fromIndex" (TupRsingle $ ExpString $ "(" ++ ix ++ ") % " ++ sh)
    let idxExp = convertVarName2ExpString newIdx
    let newIxStatement = ix ++ " = (" ++ ix ++ ") / " ++ sh ++ ";\n"
    (ixStatement, ixExp') <- compileFromIndex shr shs (ExpString ix)
    return (newIdxStatement ++ newIxStatement ++ ixStatement, TupRpair ixExp' idxExp)
compileFromIndex _ _ _ = error "compileFromIndex: impossible"

-- | Compile IndexSlice into TupR Expression
compileIndexSlice :: SliceIndex slix sl co sh -> ExpStringTup slix -> ExpStringTup sh -> ExpStringTup sl
compileIndexSlice SliceNil _ _ = TupRunit
compileIndexSlice (SliceAll slix) (TupRpair slx TupRunit) (TupRpair sl (TupRsingle sz)) =
  TupRpair (compileIndexSlice slix slx sl) (TupRsingle sz)
compileIndexSlice (SliceFixed slix) (TupRpair slx _) (TupRpair sl _) =
  compileIndexSlice slix slx sl
compileIndexSlice _ _ _ = error "compileIndexSlice: Impossible"

-- | Compile ToIndex into TupR Expression
compileIndexFull :: SliceIndex slix sl co sh -> ExpStringTup slix -> ExpStringTup sl -> ExpStringTup sh
compileIndexFull SliceNil _ _ = TupRunit
compileIndexFull (SliceAll slix) (TupRpair slx TupRunit) (TupRpair sl (TupRsingle sz)) =
  TupRpair (compileIndexFull slix slx sl) (TupRsingle sz)
compileIndexFull (SliceFixed slix) (TupRpair slz (TupRsingle sz)) sl =
  TupRpair (compileIndexFull slix slz sl) (TupRsingle sz)
compileIndexFull _ _ _ = error "compileIndexFull: Impossible"

-- | Compile ShapeSize into Expression
compileShapeSize :: ExpStringTup t -> String
compileShapeSize TupRunit = "1"
compileShapeSize (TupRpair sh1 (TupRsingle (ExpString sh2))) = "(" ++ compileShapeSize sh1 ++ ") * " ++ sh2
compileShapeSize _ = error "compileShapeSize: Impossible"