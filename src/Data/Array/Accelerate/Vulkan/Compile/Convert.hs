{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Array.Accelerate.Vulkan.Compile.Convert where

import Data.Array.Accelerate.Representation.Elt (bytesElt)
import Data.Array.Accelerate.Representation.Type (TupR (..), TypeR)
import Data.Array.Accelerate.Type
  ( FloatingDict (..),
    FloatingType (..),
    IntegralDict (..),
    IntegralType (..),
    NumType (..),
    ScalarType (..),
    SingleType (..),
    floatingDict,
    integralDict,
  )
import Numeric (showFFloat)
import Prelude hiding (exp, init, lookup)

scalarTypeToString :: ScalarType a -> String
scalarTypeToString (SingleScalarType t) = singleTypeToString t
scalarTypeToString (VectorScalarType _) = error "scalarTypeToString: Not implemented for VectorScalarType"

singleTypeToString :: SingleType a -> String
singleTypeToString (NumSingleType t) = numTypeToString t

numTypeToString :: NumType a -> String
numTypeToString (IntegralNumType t) = integralTypeToString t
numTypeToString (FloatingNumType t) = floatingTypeToString t

integralTypeToString :: IntegralType a -> String
integralTypeToString t = case t of
  TypeInt -> case bytesElt (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeInt)))) of
    4 -> error "integralTypeToString: 32-bit GHC not supported, default Int should be Int64"
    8 -> "int64_t"
    s -> error "integralTypeToString: Int size " ++ show s ++ "-bit not supported"
  TypeInt8 -> "int8_t"
  TypeInt16 -> "int16_t"
  TypeInt32 -> "int32_t"
  TypeInt64 -> "int64_t"
  TypeWord -> case bytesElt (TupRsingle (SingleScalarType (NumSingleType (IntegralNumType TypeWord)))) of
    4 -> error "integralTypeToString: 32-bit GHC not supported, default UInt should be UInt64"
    8 -> "uint64_t"
    s -> error "integralTypeToString: UInt size " ++ show s ++ "-bit not supported"
  TypeWord8 -> "uint8_t"
  TypeWord16 -> "uint16_t"
  TypeWord32 -> "uint32_t"
  TypeWord64 -> "uint64_t"

floatingTypeToString :: FloatingType a -> String
floatingTypeToString t = case t of
  TypeHalf -> "float16_t"
  TypeFloat -> "float32_t"
  TypeDouble -> "float64_t"

typeRToString :: TypeR a -> String
typeRToString (TupRsingle (SingleScalarType t)) = singleTypeToString t
typeRToString _ = undefined

numConstToString :: NumType a -> a -> String
numConstToString (IntegralNumType t) n
  | IntegralDict <- integralDict t =
      case t of
        TypeInt8 -> integralTypeToString t ++ "(" ++ show n ++ ")"
        TypeInt16 -> show n ++ "S"
        TypeInt32 -> show n
        TypeInt64 -> show n ++ "L"
        -- !NOTE: 64-bit GHC Int is 8 bytes, we assume the user is using 64-bit GHC
        TypeInt -> show n ++ "L"
        TypeWord8 -> integralTypeToString t ++ "(" ++ show n ++ ")"
        TypeWord16 -> show n ++ "US"
        TypeWord32 -> show n
        TypeWord64 -> show n ++ "UL"
        -- !NOTE: 64-bit GHC UInt is 8 bytes, we assume the user is using 64-bit GHC
        TypeWord -> show n ++ "UL"
numConstToString (FloatingNumType t) n
  | FloatingDict <- floatingDict t =
      case t of
        TypeHalf -> showFFloat Nothing n "HF"
        TypeFloat -> showFFloat Nothing n "F"
        TypeDouble -> showFFloat Nothing n "LF"
