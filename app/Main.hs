{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Array.Accelerate hiding (Eq)
import Prelude as Pre hiding (identity, const, uncurry, curry, (+), (^^), (^), lcm, gcd, (||), (!!), not, iterate, scaleFloat, isNaN, isInfinite, isDenormalized, isNegativeZero, atan2, isIEEE, significand, exponent, encodeFloat, decodeFloat, floatRange, floatDigits, floatRadix, properFraction, floor, ceiling, round, toRational, compare, min, (==), scanr1, scanr, scanl1, scanl, Ord, maximum, minimum, product, or, and, any, all, max, odd, even, reverse, Num, drop, take, tail, init, replicate, unzip3, unzip, zip, zipWith3, zip3, (<=), (>), filter, (&&), (>=), subtract, (<), truncate, fromIntegral, map, (+))

-- import Data.Array.Accelerate.Vulkan.Operation ()
-- import Data.Array.Accelerate.Vulkan.Desugar ()
import Data.Array.Accelerate.Vulkan.Kernel (VulkanKernel)
import Data.Array.Accelerate.Vulkan.Execute (Vulkan)
-- import Data.Array.Accelerate.Pretty.Schedule ()
import Data.Array.Accelerate.AST.Schedule.Sequential (SequentialSchedule)
import Data.Array.Accelerate.Pretty.Schedule.Sequential ()
-- import Data.Array.Accelerate.Data.Bits (Bits(..), FiniteBits (..))
-- import GHC.Float (float2Int)
import Data.Array.Accelerate.LLVM.Native (Native, NativeKernel)
-- import Data.Array.Accelerate.Interpreter (Interpreter)
import System.IO (hGetEncoding, mkTextEncoding, hSetEncoding, stdout, stdin, stderr)
-- import Codec.Picture (PaletteCreationMethod(Uniform))
import Data.Array.Accelerate.AST.Schedule.Uniform (UniformScheduleFun)

makeSafe h = do
  ce' <- hGetEncoding h
  case ce' of
    Nothing -> return ()
    Just ce -> mkTextEncoding (takeWhile (Pre./= '/') (show ce) Pre.++ "//TRANSLIT") >>=
      hSetEncoding h

main :: IO ()
main = do
    mapM_ makeSafe [stdout, stdin, stderr]
    -- let xs = fromList (Z:.10:.10) [100..] :: Array (Z :. Int :. Int) Int
    -- let ys = fromList (Z :. 10 :. 10) [(x*2, x*2+1) | x <- [0,1..]] :: Matrix (Int, Int)
    -- let zs = fromList (Z :. 100) [0..] :: Vector Int
    -- putStrLn $ test @SequentialSchedule @VulkanKernel (generate (I1 10) (\(I1 i) -> ifThenElse (i == 5) (i*i) (mod i 5)))
    -- print (run @Vulkan (generate (I2 10 5) (\(I2 i j) -> T2 (use zs !! toIndex (I2 10 5) (I2 i j)) (T2 i j))))


    let histogram :: Num a => Acc (Vector Int) -> Acc (Vector a)
        histogram xs =
            let zeros = fill (constant (Z:.10)) 0
                ones  = fill (shape xs)         1
            in
            permute (+) zeros (\ix -> Just_ (I1 (xs!ix))) ones

    let xs' = fromList (Z :. 20) [0,0,1,2,1,1,2,4,8,3,4,9,8,3,2,5,5,3,1,2] :: Vector Int
    putStrLn $ test @SequentialSchedule @VulkanKernel (histogram (use xs') :: Acc (Vector Int))
    print (run @Vulkan (histogram (use xs') :: Acc (Vector Int)))

    -- let convolve5x1 :: [Exp Half] -> Stencil5x1 Half -> Exp Half
    --     convolve5x1 kernel (_, (a,b,c,d,e), _)
    --         = Pre.sum $ Pre.zipWith (*) kernel [a,b,c,d,e]

    -- let convolve1x5 :: [Exp Half] -> Stencil1x5 Half -> Exp Half
    --     convolve1x5 kernel ((_,a,_), (_,b,_), (_,c,_), (_,d,_), (_,e,_))
    --         = Pre.sum $ Pre.zipWith (*) kernel [a,b,c,d,e]

    -- let gaussian = [0.06136,0.24477,0.38774,0.24477,0.06136]

    -- let blur :: Acc (Matrix Half) -> Acc (Matrix Half)
    --     blur = stencil (convolve5x1 gaussian) clamp
    --             . stencil (convolve1x5 gaussian) clamp
    
    -- let mat = fromList (Z :. 4 :. 10) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 1, 3, 5, 7, 9, 11, 13, 15, 17, 19] :: Matrix Half

    -- putStrLn $ test @SequentialSchedule @VulkanKernel (blur $ use mat)
    -- putStrLn $ test @UniformScheduleFun @NativeKernel (blur $ use mat)
    -- print (runN @Native (blur $ use mat))
    -- print (runN @Vulkan (blur $ use mat))

    -- let xs = fromList (Z:.100) [100..] :: Array (Z :. Int) Int16
    -- print (run @Vulkan (permute (*) (fill (I1 100) 1) (\(I1 i) -> Just_ (I1 i)) (use xs)))

    -- let identity :: Num a => Exp Int -> Acc (Matrix a)
    --     identity n =
    --         let zeros = fill (I2 n n) 0
    --             ones  = fill (I1 n)   1
    --         in
    --         permute const zeros (\(I1 i) -> Just_ (I2 i i)) ones
    -- putStrLn $ test @SequentialSchedule @VulkanKernel (identity 5 :: Acc (Matrix Half))
    -- print (run @Vulkan (identity 5 :: Acc (Matrix Half)))

    -- let dotp :: Num a => Vector a -> Vector a -> Acc (Scalar a)
    --     dotp xs ys =
    --         let
    --             xs' = use xs
    --             ys' = use ys
    --         in
    --         fold (+) 0 ( zipWith (*) xs' ys' )

    -- let x = fromList (Z:.10) [0..] :: Vector Float
    -- let y = fromList (Z:.10) [1,3..] :: Vector Float

    -- putStrLn $ test @SequentialSchedule @VulkanKernel (dotp x y)
    -- print (run @Vulkan (dotp x y))
    
    -- putStrLn $ test @UniformScheduleFun @NativeKernel (awhile (\x -> unit $ x!I1 0 <= 10) (map (+ 1)) (use $ fromList (Z:.10) [0..] :: Acc (Array DIM1 Int64)) :: Acc (Array DIM1 Int64))
    -- print (run @Native (awhile (\x -> unit $ x!I1 0 <= 10) (map (+ 1)) (use $ fromList (Z:.10) [0..] :: Acc (Array DIM1 Int64)) :: Acc (Array DIM1 Int64)))

    -- let primes :: Exp Int -> Acc (Vector Int)
    --     primes n = afst loop
    --       where
    --         c0    = unit 2
    --         a0    = use $ fromList (Z:.0) []
    --         limit = truncate (sqrt (fromIntegral (n+1) :: Exp Float))
    --         loop  = awhile
    --                   (\(T2 _   c) -> map (< n+1) c)
    --                   (\(T2 old c) ->
    --                     let c1 = the c
    --                         c2 = c1 < limit ? ( c1*c1, n+1 )
    --                         --
    --                         sieves =
    --                           let sz p    = (c2 - p) `quot` p
    --                               get p i = (2+i)*p
    --                           in
    --                           map (subtract c1) (expand sz get old)
    --                         --
    --                         new =
    --                           let m     = c2-c1
    --                               put i = let s = sieves ! i
    --                                       in s >= 0 && s < m ? (Just_ (I1 s), Nothing_)
    --                           in
    --                           afst
    --                             $ filter (> 0)
    --                             $ permute const (enumFromN (I1 m) c1) put
    --                             $ fill (shape sieves) 0
    --                     in
    --                     T2 (old Data.Array.Accelerate.++ new) (unit c2))
    --                   (T2 a0 c0)
    -- putStrLn $ test @UniformScheduleFun @NativeKernel (primes 4)
    -- print (run @Native (primes 6))

    -- putStrLn $ test @SequentialSchedule @VulkanKernel (expand (const 1) (\x _ -> T2 x x) (use zs))
    -- print (run @Vulkan (expand (const 1) (\x _ -> T2 x x) (use zs)))

    -- putStrLn $ test @SequentialSchedule @VulkanKernel (generate (I3 10 5 2) (\(I3 i j k) -> T3 i j k))
