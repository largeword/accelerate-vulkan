{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Physics where

import Data.Array.Accelerate

data ThreeFloats = Three_ Float Float Float
  deriving (Generic, Elt, Show)

type Mass = Float

type Position = ThreeFloats

type Acceleration = ThreeFloats

type Velocity = ThreeFloats

data PointMass = PointMass_ Position Mass
  deriving (Generic, Elt, Show)

data Body = Body_ Position Mass Velocity
  deriving (Generic, Elt, Show)

mkPatterns [''ThreeFloats, ''PointMass, ''Body]

instance Prelude.Num (Exp ThreeFloats) where
  (+) = match \(Three a b c) (Three x y z) -> Three (a + x) (b + y) (c + z)
  (-) = match \(Three a b c) (Three x y z) -> Three (a - x) (b - y) (c - z)
  (*) = match \(Three a b c) (Three x y z) -> Three (a * x) (b * y) (c * z)
  negate = match \(Three a b c) -> Three (negate a) (negate b) (negate c)
  abs = match \(Three a b c) -> Three (abs a) (abs b) (abs c)
  signum = match \(Three a b c) -> Three (signum a) (signum b) (signum c)
  fromInteger i = Three (fromInteger i) (fromInteger i) (fromInteger i)

dot :: Exp ThreeFloats -> Exp ThreeFloats -> Exp Float
dot = match $ \(Three a b c) (Three x y z) -> a * x + b * y + c * z

scale :: Exp Float -> Exp ThreeFloats -> Exp ThreeFloats
scale s = match $ \(Three a b c) -> Three (s * a) (s * b) (s * c)

epsilon :: Exp Float
epsilon = constant 1e-9

pointmass :: Exp Body -> Exp PointMass
pointmass = match \case
  Body p m _ -> PointMass p m

accel :: Exp PointMass -> Exp PointMass -> Exp Velocity
accel = match \(PointMass xpos _) (PointMass ypos ymass) ->
  let r = ypos - xpos
      rsqr = dot r r + epsilon -- ???
      inv_dist = constant 1 / sqrt rsqr
      inv_dist3 = inv_dist * inv_dist * inv_dist
      s = ymass * inv_dist3
   in scale s r

advance_body :: Exp Float -> Exp Body -> Exp Acceleration -> Exp Body
advance_body = match $ \time_step (Body pos mass vel) acc ->
  let position = pos + scale time_step vel
      velocity = vel + scale time_step acc
   in Body position mass velocity
