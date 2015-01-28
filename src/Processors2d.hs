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
--{-# LANGUAGE GADTs -}
{-# LANGUAGE FlexibleInstances #-}

module Processors2d (


 identity_v_r
,identity_v_i
,identity_v_b
,identity_v
,Matrix'(..)

--,Processor2d(..)
--,p2dpack
) where

import qualified Data.Matrix as DMatrix
import Image_loading
import qualified Codec.Picture as CPic
import Data.Matrix
import Data.Word


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




class Matrix' p where
   identity :: Matrix' p => p -> p --(DMatrix.Matrix Rational)
   toString :: Matrix' p => p -> String



instance Matrix' (DMatrix.Matrix Rational) where
   identity mtr = mtr
   toString mtr = matrix_RationalToString mtr

instance Matrix' (DMatrix.Matrix Int) where
   identity mtr = mtr
   toString mtr = matrix_IntegralToString mtr

instance Matrix' (DMatrix.Matrix Word8) where
   identity mtr = mtr
   toString mtr = matrix_IntegralToString mtr


