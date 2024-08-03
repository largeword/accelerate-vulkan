{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion
import Criterion.Main
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.AST.Schedule.Sequential (SequentialSchedule)
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.Pretty.Schedule.Sequential ()
import Data.Array.Accelerate.Vulkan.Execute (Vulkan)
import Flash_alg1
import Naive

main :: IO ()
main = do
  defaultMain
    [ backend "CPU" $ A.runN @CPU.Native,
      backend "Vulkan" $ A.runN @Vulkan
    ]
  where
    backend name runN =
      bgroup name $
        map (testcase runN) $
          (,,)
            <$> [512, 1024, 2048, 4096]
            <*> [64, 128]
            <*> [8, 64, 512, 2048, 8192, 16384]
    testcase runN (n, d, m) =
      env (pure $ runN totalProgram) $
        \p ->
          bench ("n" ++ show n ++ ", d" ++ show d ++ ", m" ++ show m) $
            nf
              p
              ( A.fromList A.Z [n],
                A.fromList A.Z [d],
                A.fromList A.Z [m]
              )
