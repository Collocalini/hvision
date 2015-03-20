-----------------------------------------------------------------------------
--
-- Module      :  Processors2d
-- Copyright   :
-- License     :  PublicDomain
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
--{-# LANGUAGE FlexibleInstances, FlexibleContexts, ExistentialQuantification #-}
 {-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Processors2d (


 identity_v_r
,identity_v_i
,identity_v_b
,identity_v
,Matrix'(..)
,Matrix''(..)
,frame_difference_vs_b
,frame_difference_vs_gsrgb8
,derivative_i_tapering_stacking_ready
,derivative_i_fork_stacking_ready
--,Processor_vs

--,Processor2d(..)
--,p2dpack
) where

import qualified Data.Matrix as DMatrix
import Data.Dynamic
import Image_loading
import qualified Codec.Picture as CPic
import Data.Matrix
import Data.Word
import Control.Monad.State
import Processors_common

{-- ================================================================================================
================================================================================================ --}
identity_v_r :: (DMatrix.Matrix Rational) -> (DMatrix.Matrix Rational)
identity_v_r  row = Processors2d.identity row
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
identity_v_i :: (DMatrix.Matrix Int) -> (DMatrix.Matrix Int)
identity_v_i  row = Processors2d.identity row
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
identity_v_b :: (DMatrix.Matrix Word8) -> (DMatrix.Matrix Word8)
identity_v_b  row = Processors2d.identity row
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
identity_v :: Matrix' a => a -> a
identity_v  row = Processors2d.identity row
----------------------------------------------------------------------------------------------------


--data  Num a => FrameDifference a = Matrix a

{-- ================================================================================================
================================================================================================ --}
frame_difference_vs :: Matrix'' a b => a -> State b a
frame_difference_vs  mtr1 = Processors2d.frame_difference mtr1
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
frame_difference_vs_b :: (DMatrix.Matrix Word8) -> State (DMatrix.Matrix Word8) (DMatrix.Matrix Word8)
frame_difference_vs_b  mtr1 = Processors2d.frame_difference mtr1
----------------------------------------------------------------------------------------------------

{-- ================================================================================================
================================================================================================ --}
frame_difference_vs_gsrgb8 :: (CPic.Image CPic.PixelRGB8) ->
                        State (CPic.Image CPic.PixelRGB8) (CPic.Image CPic.PixelRGB8)
frame_difference_vs_gsrgb8  mtr1 = Processors2d.frame_difference mtr1
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
frame_difference_gsrgb8 :: CPic.Image CPic.PixelRGB8 ->
                           CPic.Image CPic.PixelRGB8 ->
                           CPic.Image CPic.PixelRGB8
frame_difference_gsrgb8  mtr1@(CPic.Image {CPic.imageWidth  = w
                                          ,CPic.imageHeight = h}) mtr2 = CPic.generateImage
   (\x y -> step1 (CPic.pixelAt mtr1 x y) (CPic.pixelAt mtr2 x y))
   w h
   where
   step1 :: CPic.PixelRGB8 -> CPic.PixelRGB8 -> CPic.PixelRGB8
   step1 (CPic.PixelRGB8 rl8 gl8 bl8) (CPic.PixelRGB8 rr8 gr8 br8) = p (gsl - gsr)
     where
     rfl = fromIntegral rl8
     gfl = fromIntegral gl8
     bfl = fromIntegral bl8
     gsl = round ((rfl+gfl+bfl) / 3)

     rfr= fromIntegral rr8
     gfr= fromIntegral gr8
     bfr= fromIntegral br8
     gsr = round ((rfr+gfr+bfr) / 3)

     p :: Int -> CPic.PixelRGB8
     p n
        |n<0 = CPic.PixelRGB8 (fromIntegral $ abs n) 0 0
        |n>0 = CPic.PixelRGB8 0 (fromIntegral $ abs n) 0
        |n==0 = CPic.PixelRGB8 0 0 0
        |otherwise = CPic.PixelRGB8 0 0 0
----------------------------------------------------------------------------------------------------

{-- ================================================================================================
================================================================================================ --}
frame_difference_gsrgb8' :: (CPic.Image CPic.PixelRGB8) ->
                      State (CPic.Image CPic.PixelRGB8) (CPic.Image CPic.PixelRGB8)
frame_difference_gsrgb8'  mtr1@(CPic.Image {CPic.imageWidth  = w
                                           ,CPic.imageHeight = h}) = do
   mtr2@(CPic.Image {CPic.imageWidth  = w'
                    ,CPic.imageHeight = h'}) <- get
   put mtr1
   case right_size w' h' of
      True -> return $ frame_difference_gsrgb8 mtr1 mtr2
      False -> return $ img
   where
   right_size w' h' = (w<=w') && (h<=h')
   img = CPic.generateImage (\x y -> CPic.PixelRGB8 0 0 0) w h
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
frame_difference' :: (DMatrix.Matrix Word8) -> (DMatrix.Matrix Word8) -> (DMatrix.Matrix Word8)
frame_difference'  mtr1 mtr2 = DMatrix.matrix w h (\c -> fromIntegral $ abs $ (fromIntegral $ mtr1 ! c) - (fromIntegral $ mtr2 ! c))
   where
   --cords = [(y,x) | x <- [1..w], y <- [1..h]]
   w = nrows mtr1
   h = ncols mtr1
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
frame_difference'' :: (DMatrix.Matrix Word8) -> State (DMatrix.Matrix Word8) (DMatrix.Matrix Word8)
frame_difference''  mtr1 = do
   mtr2 <- get
   put mtr1
   case right_size mtr2 of
      True -> return $ frame_difference' mtr1 mtr2
      False -> return $ zero (nrows mtr1) (ncols mtr1)
   where
   right_size mtr2 = ((nrows mtr1)<=(nrows mtr2)) && ((ncols mtr1)<=(ncols mtr2))
----------------------------------------------------------------------------------------------------


{-
{-- ================================================================================================
================================================================================================ --}
frame_difference''' :: Processor_vs (DMatrix.Matrix Word8) (DMatrix.Matrix Word8) ->
                                          Processor_vs (DMatrix.Matrix Word8) (DMatrix.Matrix Word8)
frame_difference''' (Processor_vs mtr1 mtr2) =
   Processor_vs (frame_difference' mtr1 mtr2)  mtr1
----------------------------------------------------------------------------------------------------

-}



{-- ================================================================================================
================================================================================================ --}
derivative_i_tapering :: [Integer] -> [[Integer]]
derivative_i_tapering  [] = []
derivative_i_tapering  row = row : derivative_i_tapering (step1 row)
     where
        step1 :: [Integer] -> [Integer]
        step1  [] = []
        step1 (_:[]) = []
        step1 (x:rest) = ((\x' -> x-x') $ head rest):(step1 rest)
----------------------------------------------------------------------------------------------------

{--    |
       |
       |
       |
       V  --}

{-- ================================================================================================
================================================================================================ --}
derivative_i_tapering_dyn :: [Dynamic] -> [[Processor_data]]
derivative_i_tapering_dyn  row =
  map (
      map (\(x) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Integer) ) ) )
      ) $
  derivative_i_tapering $
  map (\(x) -> (fromDyn x (0:: Integer) )) row
----------------------------------------------------------------------------------------------------

{--    |
       |
       |
       |
       V  --}
{-- ================================================================================================
================================================================================================ --}
derivative_i_tapering_stacking_ready :: [Dynamic] -> [[[(Processor_data, Processor_data)]]]
derivative_i_tapering_stacking_ready  []  = []
derivative_i_tapering_stacking_ready  row =

   map (\(s, x) ->
       [zip  (rowl s) x]
       --zip [1..]
       ) $ {-take 10 $ -}zip [0,0.5..] dr

   where
   rowl :: Float -> [Processor_data]
   rowl shift = map toPd' $ [shift, shift + 1 ..] -- ::[Int]
   toPd :: Integer -> Processor_data
   toPd x =  Pd (toDyn x) (show . \z -> fromDyn z (0:: Integer) )
   toPd' :: Float -> Processor_data
   toPd' x =  Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) )

   dr :: [[Processor_data]]
   dr =  derivative_i_tapering_dyn row
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
derivative_i_fork_stacking_ready :: [Dynamic] -> [[[(Processor_data, Processor_data)]]]
derivative_i_fork_stacking_ready  []  = []
derivative_i_fork_stacking_ready  row =

   map (\x ->
       [zip (map toPd $ [maximum x, minimum x]) rowl]
       --zip [1..]
       ) $ {-take 60 $b-} derivative_i_tapering $ map (\x -> fromDyn x (0:: Integer)) row

   where
   rowl :: [Processor_data]
   rowl = map toPd $ [1..] -- ::[Integer]
   toPd :: Integer -> Processor_data
   toPd x =  Pd (toDyn x) (show . \z -> fromDyn z (0:: Integer) )

----------------------------------------------------------------------------------------------------






matrix_RationalToString :: Matrix Rational -> String
matrix_RationalToString m = unlines $ step1 [1..ncols m]
   where
   --cords = [(y,x) | x <- [1..nrows m], y <- [1..ncols m]]

   step1 :: [Int] -> [String]
   step1 [y']      = [unlines $ map (\cc@(r,c) -> (show r) ++ " " ++ (show c) ++ " " ++
                         (show . fromRational . (!) m) cc ) cords']
     where
     cords' = [(x,y') | x <- [1..nrows m]]

   step1 (y':rest) = (unlines $ map (\cc@(r,c) -> (show r) ++ " " ++ (show c) ++ " " ++
                         (show . fromRational . (!) m) cc ) cords'):(step1 rest)
     where
     cords' = [(x,y') | x <- [1..nrows m]]




matrix_FloatToString :: Matrix Float -> String
matrix_FloatToString m = unlines $ step1 [1..ncols m]
   where
   --cords = [(y,x) | x <- [1..nrows m], y <- [1..ncols m]]

   step1 :: [Int] -> [String]
   step1 [y']      = [unlines $ map (\cc@(r,c) -> (show r) ++ " " ++ (show c) ++ " " ++
                         (show . (!) m) cc ) cords']
     where
     cords' = [(x,y') | x <- [1..nrows m]]

   step1 (y':rest) = (unlines $ map (\cc@(r,c) -> (show r) ++ " " ++ (show c) ++ " " ++
                         (show . (!) m) cc ) cords'):(step1 rest)
     where
     cords' = [(x,y') | x <- [1..nrows m]]






matrix_IntegralToString :: Integral a => Matrix a -> String
matrix_IntegralToString m = unlines $ step1 [1..ncols m]
   where
   --cords = [(y,x) | x <- [1..nrows m], y <- [1..ncols m]]

   step1 :: [Int] -> [String]
   step1 [y']      = [unlines $ map (\cc@(r,c) -> (show r) ++ " " ++ (show c) ++ " " ++
                         (show . fromIntegral . (!) m) cc ) cords']
     where
     cords' = [(x,y') | x <- [1..nrows m]]

   step1 (y':rest) = (unlines $ map (\cc@(r,c) -> (show r) ++ " " ++ (show c) ++ " " ++
                         (show . fromIntegral . (!) m) cc ) cords'):(step1 rest)
     where
     cords' = [(x,y') | x <- [1..nrows m]]




--instance Fractional CPic.Pixel8 where
--   fromRational :: Fractional a => a -> CPic.Pixel8



matrix_RationalToImageY8 :: Matrix Rational -> CPic.Image CPic.Pixel8
matrix_RationalToImageY8 m =
   CPic.generateImage (\x y -> round $ m ! (x,y)) (ncols m) (nrows m)

matrix_RationalToImageY16 :: Matrix Rational -> CPic.Image CPic.Pixel16
matrix_RationalToImageY16 m =
   CPic.generateImage (\x y -> round $ m ! (x,y)) (ncols m) (nrows m)

matrix_FloatToImageY8 :: Matrix Float -> CPic.Image CPic.Pixel8
matrix_FloatToImageY8 m =
   CPic.generateImage (\x y -> round $ m ! (x,y)) (ncols m) (nrows m)


matrix_FloatToImageY16 :: Matrix Float -> CPic.Image CPic.Pixel16
matrix_FloatToImageY16 m =
   CPic.generateImage (\x y -> round $ m ! (x,y)) (ncols m) (nrows m)



matrix_IntegralToImageY8 :: Integral a => Matrix a -> CPic.Image CPic.Pixel8
matrix_IntegralToImageY8 m =
   CPic.generateImage (\x y -> fromIntegral $ m ! (x,y)) (ncols m) (nrows m)



matrix_IntegralToImageY16 :: Integral a => Matrix a -> CPic.Image CPic.Pixel16
matrix_IntegralToImageY16 m =
   CPic.generateImage (\x y -> fromIntegral $ m ! (x,y)) (ncols m) (nrows m)


matrix_IntegralToImageRGB8 :: Integral a => Matrix a -> CPic.Image CPic.PixelRGB8
matrix_IntegralToImageRGB8 m =
   CPic.generateImage (\x y -> (\c -> (CPic.PixelRGB8 c c c)) $ fromIntegral $ m ! (x+1,y+1)) (nrows m)(ncols m)





class Matrix' p where
   identity :: Matrix' p => p -> p --(DMatrix.Matrix Rational)
   toString :: Matrix' p => p -> String
   toImageY8 :: Matrix' p => p -> CPic.Image CPic.Pixel8
   toImageY16 :: Matrix' p => p -> CPic.Image CPic.Pixel16
   toImageRGB8 :: Matrix' p => p -> CPic.Image CPic.PixelRGB8
  -- frame_difference :: Matrix' p => p -> State a p

instance Matrix' (DMatrix.Matrix Rational) where
   identity mtr = mtr
   toString mtr = matrix_RationalToString mtr
   toImageY8 mtr = matrix_RationalToImageY8 mtr
   toImageY16 mtr = matrix_RationalToImageY16 mtr

instance Matrix' (DMatrix.Matrix Float) where
   identity mtr = mtr
   toString mtr = matrix_FloatToString mtr
   toImageY8 mtr = matrix_FloatToImageY8 mtr
   toImageY16 mtr = matrix_FloatToImageY16 mtr

instance Matrix' (DMatrix.Matrix Int) where
   identity mtr = mtr
   toString mtr = matrix_IntegralToString mtr
   toImageY8 mtr = matrix_IntegralToImageY8 mtr
   toImageY16 mtr = matrix_IntegralToImageY16 mtr

instance Matrix' (DMatrix.Matrix Word8) where
   identity mtr = mtr
   toString mtr = matrix_IntegralToString mtr
   toImageY8 mtr = matrix_IntegralToImageY8 mtr
   toImageY16 mtr = matrix_IntegralToImageY16 mtr
   toImageRGB8 mtr = matrix_IntegralToImageRGB8 mtr
   --frame_difference mtr = frame_difference'' mtr



class Matrix'' p s where
  -- identity :: Matrix' p => p -> p --(DMatrix.Matrix Rational)
  -- toString :: Matrix' p => p -> String
  -- toImageY8 :: Matrix' p => p -> CPic.Image CPic.Pixel8
  -- toImageY16 :: Matrix' p => p -> CPic.Image CPic.Pixel16
  -- toImageRGB8 :: Matrix' p => p -> CPic.Image CPic.PixelRGB8
   frame_difference :: Matrix'' p s => p -> State s p

instance Matrix'' (DMatrix.Matrix Word8) (DMatrix.Matrix Word8) where
   frame_difference mtr = frame_difference'' mtr


instance Matrix'' (CPic.Image CPic.PixelRGB8) (CPic.Image CPic.PixelRGB8) where
   frame_difference mtr = frame_difference_gsrgb8' mtr


--instance Processor_vs (DMatrix.Matrix Word8) (DMatrix.Matrix Word8)

--data Processor_vs a b where
--   Processor_vs :: a -> b -> Processor_vs a b

--type Processor_vs_w8 a = Processor_vs (DMatrix.Matrix Word8) a


--data Processors_vs_w8 (Processor_vs_w8 a) =
--   Processors_vs_w8 [Processor_vs_w8 a]









