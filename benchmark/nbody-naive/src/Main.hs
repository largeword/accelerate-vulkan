{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion
import Criterion.Main
import Data.Array.Accelerate
import Data.Array.Accelerate.AST.Schedule.Sequential (SequentialSchedule)
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.Pretty.Schedule.Sequential ()
import Data.Array.Accelerate.Vulkan.Execute (Vulkan)
import Debug.Trace
import Input (gen_input)
import NBody
import Physics (pointmass)

-- read input, run accelerate, benchmark
main :: IO ()
main = do
  defaultMain
    [ backend "CPU" $ runN @CPU.Native,
      backend "Vulkan" $ runN @Vulkan
    ]
  where
    backend s r = bgroup s $ Prelude.map (size r) [500, 1000, 1500, 2000, 2500, 3000, 3500, 4000]
    size r n = env (return (r nbody, fromList Z [0.1], fromList Z [n], fromList Z [10])) $ \ ~(p, dt, n, k) -> bench (show n) $ nf (p dt n) k
