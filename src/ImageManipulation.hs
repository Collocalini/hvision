
module ImageManipulation (
pixels_along_the_line,
pixels_along_the_line',
pixels_along_the_line_maybe',
coordinates_along_the_line,
coordinates_along_the_line_f

) where

import qualified Codec.Picture as CPic
import Data.List

import Global


{-- ==============================================================================================
  ============================================================================================== --}
coordinates_along_the_line :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
coordinates_along_the_line a@(x_1, y_1) b@(x_2, y_2) =
    map (\(x,y) -> (round x, round y)) $ zip [x_1',x_1'+dx .. x_2'] [y_1',y_1'+dy .. y_2']
 {--  |x_1 < x_2 = map f [x_1',x_1'+dx .. x_2']
   |x_1 > x_2 = map f [x_1',x_1'-dx .. x_2']
   |x_1 == x_2 && y_1 < y_2 = []--map f [x_1',x_1'+d .. x_2']
   |x_1 == x_2 && y_1 > y_2=  []
   |otherwise = []--}
        where
        dx = (x_2' - x_1') / (sqrt $ (abs $ x_2' - x_1')^2 + (abs $ y_2' - y_1')^2)
        dy = (y_2' - y_1') / (sqrt $ (abs $ x_2' - x_1')^2 + (abs $ y_2' - y_1')^2)

        x_1' = fromIntegral x_1
        x_2' = fromIntegral x_2
        y_1' = fromIntegral y_1
        y_2' = fromIntegral y_2

        {--m :: Float -> Float -> Float -> Float -> Float
        m x1 y1 x2 y2 = (y2-y1)/(x2-x1)
        b :: Float -> Float -> Float -> Float
        b x1 y1 m     = y1 - m*x1



        m1 = m x_1' y_1' x_2' y_2'
        b1 = b x_1' y_1' m1

        f :: Float -> (Int, Int)
        f x = (round x, round $ m1*x + b1)--}

----------------------------------------------------------------------------------------------------



{-- ==============================================================================================
  ============================================================================================== --}
coordinates_along_the_line_f :: (Float, Float) -> (Float, Float) -> [(Float, Float)]
coordinates_along_the_line_f a@(x_1, y_1) b@(x_2, y_2) =
                                                       zip [x_1, x_1+dx .. x_2] [y_1, y_1+dy .. y_2]
        where
        dx = (x_2 - x_1) / (sqrt $ (abs $ x_2 - x_1)^2 + (abs $ y_2 - y_1)^2)
        dy = (y_2 - y_1) / (sqrt $ (abs $ x_2 - x_1)^2 + (abs $ y_2 - y_1)^2)
----------------------------------------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
pixels_along_the_line :: CPic.Image CPic.Pixel8 -> [(Int, Int)] -> [[(Float, Float)]]
pixels_along_the_line img line = [map (\(x,y) -> (fromIntegral x, fromIntegral y) ) line ,
                                          zip [1 .. ] (map step1 line)]
    --[1 .. fromIntegral $ length line]
  where
  step1 :: (Int, Int) -> Float
  step1 (x,y) = fromIntegral $ CPic.pixelAt img x y
----------------------------------------------------------------------------------------------------

{-- ================================================================================================
================================================================================================ --}
pixels_along_the_line' :: CPic.Image CPic.Pixel8 -> (Int, Int) -> (Int, Int) -> [[(Float, Float)]]
pixels_along_the_line' img a b = pixels_along_the_line img $ coordinates_along_the_line a b
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
pixels_along_the_line_maybe' :: Maybe (CPic.Image CPic.Pixel8) -> (Int, Int) -> (Int, Int) ->
                                                                                [[(Float, Float)]]
pixels_along_the_line_maybe' Nothing _ _ = []
pixels_along_the_line_maybe' (Just img) a b = pixels_along_the_line' img a b
----------------------------------------------------------------------------------------------------





