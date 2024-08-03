{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Array.Accelerate
  ( Acc,
    Int8,
    Num,
    Vector,
    Z (Z),
    constant,
    fill,
    fromList,
    permute,
    run,
    shape,
    test,
    use,
    (!),
    pattern I1,
    pattern Just_,
    type (:.) ((:.)),
  )
import Data.Array.Accelerate.AST.Schedule.Sequential (SequentialSchedule)
import Data.Array.Accelerate.Pretty.Schedule.Sequential ()
import Data.Array.Accelerate.Vulkan.Execute (Vulkan)
import Data.Array.Accelerate.Vulkan.Kernel (VulkanKernel)
import System.IO (hGetEncoding, hSetEncoding, mkTextEncoding, stderr, stdin, stdout)
import System.Mem (performGC)
import Prelude

makeSafe h = do
  ce' <- hGetEncoding h
  case ce' of
    Nothing -> return ()
    Just ce ->
      mkTextEncoding (takeWhile (/= '/') (show ce) ++ "//TRANSLIT")
        >>= hSetEncoding h

main :: IO ()
main = do
  -- Set the terminal text encoding to UTF-8
  mapM_ makeSafe [stdout, stdin, stderr]

  let histogram :: (Data.Array.Accelerate.Num a) => Acc (Vector Int) -> Acc (Vector a)
      histogram xs =
        let zeros = fill (constant (Z :. 10)) 0
            ones = fill (shape xs) 1
         in permute (+) zeros (\ix -> Just_ (I1 (xs ! ix))) ones

  let xs' = fromList (Z :. 20) [0, 0, 1, 2, 1, 1, 2, 4, 8, 3, 4, 9, 8, 3, 2, 5, 5, 3, 1, 2] :: Vector Int
  -- Print out IRs
  putStrLn $ test @SequentialSchedule @VulkanKernel (histogram (use xs') :: Acc (Vector Int8))
  -- Run the program
  print (run @Vulkan (histogram (use xs') :: Acc (Vector Int8)))

  performGC
