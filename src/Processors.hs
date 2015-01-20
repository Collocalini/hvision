
module Processors (

identity_i,
identity_i_dyn,
identity_f,
identity_f_dyn,
identity_v_f,
derivative_i,
derivative_i_dyn,
derivative_f,
derivative_f_dyn,
max_derivative_in_range_xy_f_dyn,
min_derivative_in_range_xy_f_dyn,
distance_between_extremums_f,
distance_between_extremums_f_dyn,
extremums_f,
extremums_f_dyn,
processor_x_2_f,
processor_x_2_f_dyn,
processor_x_2_2_f,
processor_x_2_2_f_dyn,
processor_x_2_3_f,
processor_x_2_3_f_dyn,

processor_xm_2_f,
processor_xm_2_f_dyn,
processor_xm_2_2_f,
processor_xm_2_2_f_dyn,
processor_xm_2_3_f,
processor_xm_2_3_f_dyn,

frame_difference_f,
frame_difference_sequence_f,
frame_difference_sequence_f_dyn,

histogram_y_per_pixel_multiple_rows_f_dyn,
histogram_y_per_pixel_multiple_rows_dft_f_dyn,

filter_range_f_dyn,

histogram_ad_hock_f_dyn,  -- ad hock


--toStringTable_matrix_context,

--Processor_data



) where

import Data.List
import Data.Dynamic
import Data.Complex
import GHC.Float
--import qualified Data.Map as DMap
import ImageManipulation
import Global
import Control.DeepSeq
import Numeric.FFT
import Processors_common
import qualified Data.Matrix as DMatrix
--

data MarkExtremums = Max|Min|Both deriving (Eq)

data MarkRanges = Lb|Rb|Il|N deriving (Eq)

--eol_char = "\n"


------------------------------------ section of processors -----------------------------------------



{-- ================================================================================================
================================================================================================ --}
identity_i :: [(Int, Int)] -> [(Int, Int)]
identity_i  row = row

{--    |
       |
       |
       |
       V  --}
identity_i_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
identity_i_dyn  row = map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Int) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Int) ) )) $
                      identity_i $
                      map (\(x, y) -> ((fromDyn x 0) , (fromDyn y 0) )) row
---------------------------------








{-- ================================================================================================
================================================================================================ --}
identity_f :: [(Float, Float)] -> [(Float, Float)]
identity_f  row = row

{--    |
       |
       |
       |
       V  --}
identity_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
identity_f_dyn  row =
                      identity_f `deepseq`
                      map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float)),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float)) )) $
                      identity_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float , (fromDyn y (0:: Float)) )) row
---------------------------------







{-- ================================================================================================
================================================================================================ --}
identity_v_f :: (DMatrix.Matrix Rational) -> (DMatrix.Matrix Rational)
identity_v_f  row = row
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
derivative_i :: [(Int, Int)] -> [(Int, Int)]
derivative_i  [] = []
derivative_i  row@((x_prev, _):_) = (x_prev, 0):(step1 row)
     where
        step1 :: [(Int, Int)] -> [(Int, Int)]
        step1  [] = []
        step1 (_:[]) = []
        step1 ((x_prev, y_prev):rest) = ((\(x, y) -> (x , y-y_prev) ) $ head rest):
             (step1 rest)
{--    |
       |
       |
       |
       V  --}
derivative_i_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
derivative_i_dyn  row = map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Int) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Int) ) )) $
                      derivative_i $
                      map (\(x, y) -> ((fromDyn x 0):: Int , (fromDyn y 0):: Int )) row
----------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
derivative_f :: [(Float, Float)] -> [(Float, Float)]
derivative_f  [] = []
derivative_f  row@((x_prev, _):_) = (x_prev, 0):(step1 row)
     where
        step1 :: [(Float, Float)] -> [(Float, Float)]
        step1  [] = []
        step1 (_:[]) = []
        step1 ((x_prev, y_prev):rest) = ((\(x, y) -> (x_prev + ( (abs $ x - x_prev)/2),
                                                  y-y_prev) ) $ head rest):(step1 $ rest)
{--    |
       |
       |
       |
       V  --}
derivative_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
derivative_f_dyn  row =
                      derivative_f `deepseq`
                      map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      derivative_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float , (fromDyn y 0):: Float )) row
---------------------------------------------------------------------








{-- ================================================================================================
-- assumes that input is sorted by x in increasing order
================================================================================================ --}
derivative_in_range_f :: Float -> Float -> MarkExtremums -> [(Float, Float)] -> [(Float, Float)]
derivative_in_range_f _ _ _ [] = []
derivative_in_range_f l r mode (fst:row) = step1 [] fst row
   where
   step1 :: [(Float, Float)] -> (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
   step1 [] (x,_)   []            = [(x,0)]
   step1 [] (x,y)   ((x1,y1):[])  = [(x,0),(x1,y1-y)]
   step1 p  c@(x,_) n@(next:[])   = step2 $ max_in_range (adjust_range l r c) $ p ++ n
     where
     step2 :: Maybe (Float, Float) -> [(Float, Float)]
     step2 Nothing = step1 p next []
     step2 (Just (_,y)) = (x,y):(step1 p next [])


   step1 p c@(x,_) n@(next:rest)  = step2 $ max_in_range (adjust_range l r c) $ p ++ n
     where
     step2 :: Maybe (Float, Float) -> [(Float, Float)]
     step2 Nothing = step1 p next rest
     step2 (Just (_,y)) = (x,y):(step1 p next rest)


   max_in_range :: (Float, Float) -> [(Float, Float)] -> Maybe (Float, Float)
   max_in_range _ [] = Nothing
   max_in_range (l',r') row' = step1' $ filter (\(x,_)-> (x>l' && x<r') ) row'

      where
      step1' :: [(Float, Float)] -> Maybe (Float, Float)
      step1' [] = Nothing
      step1' row''
        |mode == Max = Just $ maximumBy sortGt_by_y row''
        |otherwise  = Just $ minimumBy sortGt_by_y row''

   adjust_range :: Float -> Float -> (Float, Float) -> (Float, Float)
   adjust_range l' r' (x,_) = (x+l', x+r') -- NOT A TYPO!!!

{--    |
       |
       |
       |
       V  --}

{-- ================================================================================================
================================================================================================ --}
max_derivative_in_range_xy_f :: [(Float, Float)] -> [(Float, Float)]
max_derivative_in_range_xy_f row = derivative_in_range_f (-50) 50 Max $ derivative_f row
                      --head_repeats row $
                 --     max_derivative_in_range
{--    |
       |
       |
       |
       V  --}

max_derivative_in_range_xy_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
max_derivative_in_range_xy_f_dyn  row =
                     max_derivative_in_range_xy_f `deepseq`
                     map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      max_derivative_in_range_xy_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float , (fromDyn y 0):: Float )) row
---------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
min_derivative_in_range_xy_f :: [(Float, Float)] -> [(Float, Float)]
min_derivative_in_range_xy_f row = derivative_in_range_f (-50) 50 Min $ derivative_f row
                      --head_repeats row $
                 --     max_derivative_in_range
{--    |
       |
       |
       |
       V  --}

min_derivative_in_range_xy_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
min_derivative_in_range_xy_f_dyn  row =
                     min_derivative_in_range_xy_f `deepseq`
                     map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      min_derivative_in_range_xy_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float , (fromDyn y 0):: Float )) row
---------------------------------------------------------------------

--}











{-- ================================================================================================
================================================================================================ --}
processor_x_n :: Int -> [(Float, Float)] -> [(Float, Float)]
processor_x_n _ [] = []
processor_x_n n row = do

  let big_first              = sortBy  (sortLt_by_y)   row
  let (first_n, bf_row_rest) = splitAt n               big_first
  let first_n_sorted_by_x    = sortBy  sortGt_by_x     first_n

  let (next_n, bf_row_rest')  = splitAt n              bf_row_rest
  let next_n_sorted_by_x     = sortBy  sortGt_by_x     next_n

  -- assigned to parent extremums
  let assigned               = break_to_n     first_n_sorted_by_x
                                              next_n_sorted_by_x

  let ns_with_fall_offs      = falloff        leftmost
                                              rightmost
                                              first_n_sorted_by_x

  let this_iteration = distances_to_falloff   ns_with_fall_offs
                                              assigned

  -- testing

  --let test = test_show_falloff ns_with_fall_offs

  --test ++ step1   next_n_sorted_by_x
  --               bf_row_rest'
  -- testing

  let preliminary = first_n_sorted_by_x ++ this_iteration ++ step1   next_n_sorted_by_x
                                             bf_row_rest'

  sortBy  sortGt_by_x preliminary



 -- big_first


  where
  leftmost = head row
  rightmost = last row

  {-- ==============================================================================================
  ============================================================================================== --}
  step1 :: [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)]
  step1 [] _ = []
  step1 _ [] = []
  step1 ps bf
    -- |n > length bf = do

    |otherwise = do

        let (next_n, bf_row_rest)  = splitAt  n               bf
        let next_n_sorted_by_x     = sortBy   sortGt_by_x     next_n


        -- assigned to parent extremums
        let assigned               = break_to_n   ps
                                                  next_n_sorted_by_x


        let ns_with_fall_offs      = falloff     leftmost
                                                 rightmost
                                                 ps


        let this_iteration = distances_to_falloff   ns_with_fall_offs
                                                    assigned

       -- test
        --let test = test_show_falloff ns_with_fall_offs

        --test ++ step1   next_n_sorted_by_x
        --               bf_row_rest
        -- test

        --next_n_sorted_by_x ++ --
        this_iteration ++
                         step1   next_n_sorted_by_x
                                 bf_row_rest

    -- []
  --------------------------------------------------------------------------------------------------

  {-- ==============================================================================================
  ============================================================================================== --}
  falloff :: (Float, Float) -> (Float, Float) -> [(Float, Float)] ->
                                                 [(Float, Float, (Float, Float), (Float, Float))]
  falloff _ _ [] = []
  falloff l@(lx, ly) r@(rx, ry) ((x,y):rest) = (x,y, f1, f2):(falloff l r rest)
    where
    m x1 y1 x2 y2 = (y2-y1)/(x2-x1)
    b x1 y1 m     = y1 - m*x1

    m1 = m lx ly x y
    b1 = b lx ly m1
    m2 = m x y rx ry
    b2 = b x y m2

    f1 = (m1, b1)
    f2 = (m2, b2)
  --------------------------------------------------------------------------------------------------

  {-- ==============================================================================================
  ============================================================================================== --}
  distance_to_falloff :: (Float, Float, (Float, Float), (Float, Float)) -> (Float, Float) ->
                                                                           (Float, Float)
  distance_to_falloff (px, py, (m1, b1), (m2, b2)) (nx, ny)
    |px < nx = (nx, d nx ny m1 b1)
    |px > nx = (nx, d nx ny m2 b2)
    |px == nx = (nx, d nx ny 0 py)
    |otherwise = (nx, ny)
    where
    d x y m b = (abs (y - m*x -b))/(sqrt (m^2 +1))
  --------------------------------------------------------------------------------------------------

  {-- ==============================================================================================
  ============================================================================================== --}
  distances_to_falloff :: [(Float, Float, (Float, Float), (Float, Float))] -> [[(Float, Float)]] ->
                                                                           [(Float, Float)]
  distances_to_falloff [] _ = []
  distances_to_falloff _ [] = []
  distances_to_falloff (p:prest) (n:nrest) = (map (distance_to_falloff p) n) ++
                                                                    distances_to_falloff prest nrest
  --------------------------------------------------------------------------------------------------

  test_show_falloff :: [(Float, Float, (Float, Float), (Float, Float))] -> [(Float, Float)]
  test_show_falloff [] = []
  test_show_falloff ((px, _, (m1, b1), (m2, b2)):rest) =
     (map (f m1 b1) xl) ++ (map (f m2 b2) xr) ++ test_show_falloff rest

    where
       l = (\(l',_) -> l') leftmost
       r = (\(l',_) -> l') rightmost
       xl      = [l .. px]
       xr      = [px .. r]
       f m b x = (x, m*x + b)
----------------------------------------------------------------------------------------------------

{-- ==============================================================================================
-- assumes that first_n are sorted by x coordinate
-- assumes that row_rest is sorted by x coordinate
============================================================================================== --}
break_to_n :: [(Float, Float)] -> [(Float, Float)] -> [[(Float, Float)]]
break_to_n first_n row_rest = step1 (delimeters first_n) row_rest
     where
     delimeters :: [(Float, Float)] -> [Float]
     delimeters [] = []
     delimeters [_] = []
     delimeters ((x, _):(xn, _):rest) =  ((xn - x)/2 ):(delimeters rest)

     in_range :: Float -> (Float, Float) -> Bool
     in_range lim (x, _) = x <= lim

     step1 :: [Float] -> [(Float, Float)] -> [[(Float, Float)]]
     step1 [] row = [row]
     step1 (lim:rest) row = (\(l,r) -> l:(step1 rest r) ) $ span (in_range lim) row
--------------------------------------------------------------------------------------------------


{--    |
       |
       |
       |
       V  --}
{-- ================================================================================================
================================================================================================ --}
processor_x_2_f :: [(Float, Float)] -> [(Float, Float)]
processor_x_2_f row = --head_repeats row $
                      processor_x_n 2 $ mark_extremums Max row
----------------------------------------------------------------------------------------------------
{--    |
       V  --}
{-- ================================================================================================
================================================================================================ --}
processor_x_2_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
processor_x_2_f_dyn  row =map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      processor_x_2_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
----------------------------------------------------------------------------------------------------
{--    |
       V  --}
{-- ================================================================================================
================================================================================================ --}
processor_x_2_2_f :: [(Float, Float)] -> [(Float, Float)]
processor_x_2_2_f row =  --head_repeats row $
                         processor_x_n 2 $
                         mark_extremums Max $
                         processor_x_n 2 $ mark_extremums Max row
----------------------------------------------------------------------------------------------------
{--    |
       V  --}
{-- ================================================================================================
================================================================================================ --}
processor_x_2_2_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
processor_x_2_2_f_dyn  row =map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      processor_x_2_2_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
----------------------------------------------------------------------------------------------------
{--    |
       V  --}
{-- ================================================================================================
================================================================================================ --}
processor_x_2_3_f :: [(Float, Float)] -> [(Float, Float)]
processor_x_2_3_f row =  --head_repeats row $
                         processor_x_n 2 $
                         mark_extremums Max $
                         processor_x_n 2 $
                         mark_extremums Max $
                         processor_x_n 2 $ mark_extremums Max row
----------------------------------------------------------------------------------------------------
{--    |
       V  --}
{-- ================================================================================================
================================================================================================ --}
processor_x_2_3_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
processor_x_2_3_f_dyn  row =map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      processor_x_2_3_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
----------------------------------------------------------------------------------------------------
{--    |
       |
       |
       |
       V  --}
{-- ================================================================================================
================================================================================================ --}
processor_xm_2_f :: [(Float, Float)] -> [(Float, Float)]
processor_xm_2_f row =  --head_repeats row $
                        processor_x_n 2 $ mark_extremums Min row
----------------------------------------------------------------------------------------------------
{--    |
       V  --}
{-- ================================================================================================
================================================================================================ --}
processor_xm_2_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
processor_xm_2_f_dyn  row =map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      processor_xm_2_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
----------------------------------------------------------------------------------------------------
{--    |
       V  --}
{-- ================================================================================================
================================================================================================ --}
processor_xm_2_2_f :: [(Float, Float)] -> [(Float, Float)]
processor_xm_2_2_f row = --head_repeats row $
                         processor_x_n 2 $
                         mark_extremums Min $
                         processor_x_n 2 $ mark_extremums Min row
----------------------------------------------------------------------------------------------------
{--    |
       V  --}
{-- ================================================================================================
================================================================================================ --}
processor_xm_2_2_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
processor_xm_2_2_f_dyn  row =map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      processor_xm_2_2_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
----------------------------------------------------------------------------------------------------
{--    |
       V  --}
{-- ================================================================================================
================================================================================================ --}
processor_xm_2_3_f :: [(Float, Float)] -> [(Float, Float)]
processor_xm_2_3_f row = --head_repeats row $
                         processor_x_n 2 $
                         mark_extremums Min $
                         processor_x_n 2 $
                         mark_extremums Min $
                         processor_x_n 2 $ mark_extremums Min row
----------------------------------------------------------------------------------------------------
{--    |
       V  --}
{-- ================================================================================================
================================================================================================ --}
processor_xm_2_3_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
processor_xm_2_3_f_dyn  row =map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      processor_xm_2_3_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
----------------------------------------------------------------------------------------------------



----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------









{-- ================================================================================================
================================================================================================ --}
distance_between_extremums_f :: [(Float, Float)] -> [(Float, Float)]
distance_between_extremums_f  [] = []
distance_between_extremums_f  row@((x_prev, _):_) = --step2 row step1_
                                                    step1 $ mark_extremums Both row

     where
        step1 :: [(Float, Float)] -> [(Float, Float)]
        step1  [] = []
        step1 (_:[]) = []
        step1 (prev@(x_prev, y_prev):curr@(x_curr, y_curr):rest)
           = (x_prev+dist/2, dist):(step1 $ curr:rest)
           where
              dist = abs(x_prev-x_curr)

        step1_ = step1 mark_extremums_          --- !!! shortcut
        mark_extremums_ = mark_extremums Both row    --- !!! shortcut

        in_range :: Float -> (Float, Float) -> Bool
        in_range lim (x, _) = x <= lim

{--    |
       |
       |
       |
       V  --}
distance_between_extremums_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
distance_between_extremums_f_dyn  row =
                     map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      distance_between_extremums_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
---------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
filter_range_f :: [(Float, Float)] -> [(Float, Float)]
filter_range_f  [] = []
filter_range_f  row = --step1 $
                      filter (in_range_y 1.0) $ distance_between_extremums_f row
    where

    in_range_y :: Float -> (Float, Float) -> Bool
    in_range_y lim (_, y) = y == lim

    step1 :: [(Float, Float)] -> [(Float, Float)]
    step1 [] = [(0,0)]
    step1 otherwise = otherwise

{--    |
       |
       |
       |
       V  --}
filter_range_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
filter_range_f_dyn  row =
                     map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      filter_range_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
---------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
extremums_f :: [(Float, Float)] -> [(Float, Float)]
extremums_f [] = []
extremums_f input =  --head_repeats input $
                     mark_extremums Both input
                     --step2 input $ mark_extremums input

{--    |
       |
       |
       |
       V  --}
extremums_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
extremums_f_dyn  row =map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      extremums_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
---------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
mark_extremums :: MarkExtremums -> [(Float, Float)] -> [(Float, Float)]
mark_extremums _ [] = []
mark_extremums _ (_:[]) = []
mark_extremums variant input@(prev@(x_prev, y_prev):curr@(x_curr, y_curr):rest)
  |variant == Max = step2max $ step1 input
  |variant == Min = step2min $ step1 input
  |otherwise= step2Both $ step1 input
   where
    up :: Float -> Float -> Float -> Bool
    up yp yc yn= (yp < yc) && (yn < yc)

    down :: Float -> Float -> Float -> Bool
    down yp yc yn = (yp > yc) && (yn > yc)


    -- eliminate flats
    step1 :: [(Float, Float)] -> [(Float, Float)]
    step1 [] = []
    step1 (_:[]) = []
    step1 (bef@(x_bef, y_bef):prev@(x_prev, y_prev):[])
        |(y_bef == y_prev) = [((get_middle x_bef x_prev), y_prev)]
        |otherwise = [bef, prev]
    step1 (bef@(x_bef, y_bef):prev@(x_prev, y_prev):curr@(x_curr, y_curr):rest)
        |(y_bef == y_prev) && (y_prev == y_curr) = step1 $ bef:curr:rest
        |(y_bef == y_prev) && (y_prev /= y_curr) = (middle_point bef prev):
                                                                    (step1 $ curr:curr:rest)
        |(y_bef /= y_prev) && (y_prev == y_curr) = bef:(step1 $ prev:prev:curr:rest)
        |(y_bef /= y_prev) && (y_prev /= y_curr) = bef:prev:(step1 $ curr:curr:rest)
        |otherwise = (step1 $ curr:rest)

    -- find extremums
    step2Both :: [(Float, Float)] -> [(Float, Float)]
    step2Both [] = []
    step2Both (_:[])   = []
    step2Both (_:_:[]) = []
    step2Both (prev@(x_prev, y_prev):curr@(x_curr, y_curr):next@(x_next, y_next):rest)
        |up y_prev y_curr y_next = curr:(step2Both $ curr:next:rest)
        |down y_prev y_curr y_next = curr:(step2Both $ curr:next:rest)
        |otherwise = step2Both $ curr:next:rest

    step2max :: [(Float, Float)] -> [(Float, Float)]
    step2max [] = []
    step2max (_:[]) = []
    step2max (_:_:[]) = []
    step2max (prev@(x_prev, y_prev):curr@(x_curr, y_curr):next@(x_next, y_next):rest)
        |up y_prev y_curr y_next = curr:(step2max $ curr:next:rest)
       -- |down y_prev y_curr y_next = curr:(step2max $ curr:next:rest)
        |otherwise = step2max $ curr:next:rest

    step2min :: [(Float, Float)] -> [(Float, Float)]
    step2min [] = []
    step2min (_:[]) = []
    step2min (_:_:[]) = []
    step2min (prev@(x_prev, y_prev):curr@(x_curr, y_curr):next@(x_next, y_next):rest)
       -- |up y_prev y_curr y_next = curr:(step2min $ curr:next:rest)
        |down y_prev y_curr y_next = curr:(step2min $ curr:next:rest)
        |otherwise = step2min $ curr:next:rest


    get_middle :: Float -> Float -> Float
    get_middle a b = a + (abs $ a - b)/2

    middle_point :: (Float, Float) -> (Float, Float) -> (Float, Float)
    middle_point a@(ax,ay) b@(bx,by) = ((get_middle ax bx), ay)
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
frame_difference_f :: [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)]
--frame_difference_f _ _ = [(0,0),(0,0),(0,0)]
--frame_difference_f l r = identity_f l
frame_difference_f [] _ = []
frame_difference_f _ [] = []
frame_difference_f ((x1,y1):rest1) ((x2,y2):rest2)
   |x1 == x2   = (x1, y2-y1): frame_difference_f rest1 rest2
   |otherwise = (x1, y2-y1):frame_difference_f rest1 rest2
{--   --}
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
frame_difference_sequence_f :: [[(Float, Float)]] -> [[(Float, Float)]]
frame_difference_sequence_f [] = []
frame_difference_sequence_f (h:input) = --[[(0,0),(0,0),(0,0)]]
                                    h:step1 input
   where
   step1 :: [[(Float, Float)]] -> [[(Float, Float)]]
   step1 [] = []
   step1 (f:[]) = [f]
   --step1 ([]:f1:f2:rest) = f1:(frame_difference_f f1 f2):(step1 $ f2:rest)
   step1    (f1:f2:rest) =    (frame_difference_f f1 f2):(step1 $ f2:rest)
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
frame_difference_sequence_f_dyn :: [[(Dynamic, Dynamic)]] -> [[(Processor_data, Processor_data)]]
frame_difference_sequence_f_dyn  row =
                     frame_difference_sequence_f `deepseq`
                     map (
                          map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                           Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) ))
                         ) $
                     frame_difference_sequence_f $
                     map (
                           map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float ))
                         ) row
----------------------------------------------------------------------------------------------------




{-- ==============================================================================================
============================================================================================== --}
each_bar_is_a_pixel :: Float -> Float -> [Float]
each_bar_is_a_pixel l h = [l,(l+1) .. h]
                          --[(l-0.5),(l+0.5) .. (h+0.5)]
--------------------------------------------------------------------------------------------------



{-- ==============================================================================================
-- assumes that data_ is sorted by x coordinate
============================================================================================== --}
fall_to_ranges_x :: [(Float, Float)] -> [Float] -> [[(Float, Float)]]
fall_to_ranges_x [] _ = []
fall_to_ranges_x d [] = [d]
fall_to_ranges_x data_ bars = step1 bars data_
  where

  in_range :: Float -> (Float, Float) -> Bool
  in_range lim (x, _) = x <= lim

  step1 :: [Float] -> [(Float, Float)] -> [[(Float, Float)]]
  step1 [] row = [row]
  step1 (lim:rest) row = (\(l,r) -> l:(step1 rest r) ) $ span (in_range lim) row
--------------------------------------------------------------------------------------------------


{-- ==============================================================================================
-- assumes that data_ is sorted by y coordinate
============================================================================================== --}
fall_to_ranges_y :: [(Float, Float)] -> [Float] -> [[(Float, Float)]]
fall_to_ranges_y [] _ = []
fall_to_ranges_y d [] = [d]
fall_to_ranges_y data_ bars = step1 bars data_
  where

  in_range :: Float -> (Float, Float) -> Bool
  in_range lim (_, y) = y <= lim

  step1 :: [Float] -> [(Float, Float)] -> [[(Float, Float)]]
  step1 [] row = [row]
  step1 (lim:rest) row = (\(l,r) -> l:(step1 rest r) ) $ span (in_range lim) row
--------------------------------------------------------------------------------------------------




{-- ==============================================================================================
==============================================================================================
histogram_y_per_pixel :: [(Float, Float)] -> [(Float, Float)]
histogram_y_per_pixel [] = []
histogram_y_per_pixel data_ = zip (each_bar_is_a_pixel miny maxy) $ map (fromIntegral.length) $
                          fall_to_ranges (sortBy sortGt_by_y data_) $ each_bar_is_a_pixel miny maxy
  where
  (_, miny) = minimumBy sortGt_by_y data_
  (_, maxy) = maximumBy sortGt_by_y data_
--------------------------------------------------------------------------------------------------
--}

{-- ==============================================================================================
============================================================================================== --}
histogram_y_per_pixel_multiple_rows :: [[(Float, Float)]] -> [[(Float, Float)]]
histogram_y_per_pixel_multiple_rows [] = []
histogram_y_per_pixel_multiple_rows data_ = cut_to_stripes_by_y $ cut_to_stripes_by_x data_
                                                     {--zip (each_bar_is_a_pixel minx maxx) $
                                                                     map histogram_y_per_pixel $
                                                                     fall_to_ranges
                                                                     each_bar_is_a_pixel minx maxx
                                                                     data_-}

  where

  cut_to_stripes_by_x :: [[(Float, Float)]] -> [[[(Float, Float)]]]
  cut_to_stripes_by_x data_ = map one_stripe_x data_

  one_stripe_x :: [(Float, Float)] -> [[(Float, Float)]]
  one_stripe_x [] = []
  one_stripe_x data_ = --zip (each_bar_is_a_pixel minx maxx) $ map (fromIntegral.length) $
                                                fall_to_ranges_x (sortBy sortGt_by_x data_) $
                                                each_bar_is_a_pixel minx maxx
    where
    (minx, _) = minimumBy sortGt_by_x data_
    (maxx, _) = maximumBy sortGt_by_x data_

  cut_to_stripes_by_y :: [[[(Float, Float)]]] -> [[(Float, Float)]]
  cut_to_stripes_by_y step1 = --transpose $
                              map one_stripe_y $ map concat $ transpose step1

  one_stripe_y :: [(Float, Float)] -> [(Float, Float)]
  one_stripe_y [] = []
  one_stripe_y data_ = --zip ((miny-1):(each_bar_is_a_pixel miny maxy) ++ [(maxy+1)]) $
                       zip ((each_bar_is_a_pixel miny maxy)) $
                                                map (fromIntegral.length) $
                                                fall_to_ranges_y (sortBy sortGt_by_y data_) $
                                                each_bar_is_a_pixel miny maxy
    where
    (_, miny) = minimumBy sortGt_by_y data_
    (_, maxy) = maximumBy sortGt_by_y data_

--count = map (map (map ((map length).(sortBy sortGt_by_y).concat))) $ transpose cut_to_stripes_by_y

--------------------------------------------------------------------------------------------------

hist_chain1 :: [[(Float, Float)]] -> [[(Float, Float)]]
hist_chain1 d = histogram_y_per_pixel_multiple_rows $ map distance_between_extremums_f d

hist_chain2 :: [[(Float, Float)]] -> [[(Float, Float)]]
hist_chain2 d = map fft_of_column_of_hist $ histogram_y_per_pixel_multiple_rows $
                                                                  map distance_between_extremums_f d


hist_chain3 :: [[(Float, Float)]] -> [[(Float, Float)]]
hist_chain3 d = histogram_y_per_pixel_multiple_rows $ map filter_range_f d


hist_chain4 :: [[(Float, Float)]] -> [[(Float, Float)]]
hist_chain4 d = --map fill_holes $ histogram_y_per_pixel_multiple_rows $
                  map fft_of_column_of_hist $ to_same_length $ map fill_holes $ histogram_y_per_pixel_multiple_rows $
                                                                  map distance_between_extremums_f d

{--    |
       |
       |
       |
       V  --}

{-- ================================================================================================
================================================================================================ --}
histogram_y_per_pixel_multiple_rows_f_dyn :: [[(Dynamic, Dynamic)]] ->
                                                                [[(Processor_data, Processor_data)]]
histogram_y_per_pixel_multiple_rows_f_dyn  row =
                     hist_chain1 `deepseq`
                     map (
                          map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                           Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) ))
                         ) $
                     hist_chain1 $
                     map (
                           map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float ))
                         ) row
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
histogram_y_per_pixel_multiple_rows_dft_f_dyn :: [[(Dynamic, Dynamic)]] ->
                                                                [[(Processor_data, Processor_data)]]
histogram_y_per_pixel_multiple_rows_dft_f_dyn  row =
                     hist_chain1 `deepseq`
                     map (
                          map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                           Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) ))
                         ) $
                     hist_chain2 $
                     map (
                           map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float ))
                         ) row
----------------------------------------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
histogram_ad_hock_f_dyn :: [[(Dynamic, Dynamic)]] ->
                                                                [[(Processor_data, Processor_data)]]
histogram_ad_hock_f_dyn  row =
                     --hist_chain2 `deepseq`
                     hist_chain4 `deepseq`
                     map (
                          map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                           Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) ))
                         ) $
                     --hist_chain2 $
                     hist_chain4 $
                     map (
                           map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float ))
                         ) row
----------------------------------------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
fft_of_column_of_hist:: [(Float, Float)] -> [(Float, Float)]
fft_of_column_of_hist input = ---zip l [maximum $ map (double2Float.realPart) $ dft $
                              --                                  map (\i -> float2Double i :+ 0) r]
                              zip [0..] $ map (double2Float.phase) $
                                --filter (\x -> ((magnitude x) < 20)&&((magnitude x) > -20)) $
                                                                   dft $
                                                                   map (\i -> float2Double i :+ 0) r
   where
   (l,r) = unzip input
----------------------------------------------------------------------------------------------------






{-- ================================================================================================
================================================================================================ --}
fill_holes:: [(Float, Float)] -> [(Float, Float)]
fill_holes []     = []
fill_holes row = interpolate (to_pairs $ put_brackets $ to_threes row)
                             (filter (\(_,r)-> r/=0) row)
  where
  to_threes :: [(a, a)] -> [((a,a),(a,a),(a,a))]
  to_threes [] = []
  to_threes [a] = []
  to_threes [a,b] = []
  to_threes (l:c:r:rest) = (l,c,r):(to_threes $ c:r:rest)

  put_brackets :: [((Float,Float),(Float,Float),(Float,Float))] -> [((Float,Float),MarkRanges)]
  put_brackets row = filter (\(_,r)-> r/=N) $ map rcg row


  rcg :: ((Float,Float),(Float,Float),(Float,Float)) -> ((Float,Float), MarkRanges)
  rcg ((_,lz),p@(_,pz),(_,rz))
    |(lz == 0)&&(pz /= 0)&&(rz /= 0) = (p, Lb)
    |(lz /= 0)&&(pz /= 0)&&(rz == 0) = (p, Rb)
    |(lz == 0)&&(pz /= 0)&&(rz == 0) = (p, Il)
    |otherwise = (p, N)

  to_pairs :: [((Float,Float),MarkRanges)] -> [((Float,Float), (Float,Float))]
  to_pairs (f@(_,Rb):rest) = to_pairs_step1 rest
  to_pairs row = to_pairs_step1 row

  to_pairs_step1 :: [((Float,Float),MarkRanges)] -> [((Float,Float), (Float,Float))]
  to_pairs_step1 []   = []
  to_pairs_step1 [_]  = []
  to_pairs_step1 [(ap,Rb),(bp,Lb)] = [(ap,bp)]
  to_pairs_step1 [(ap,Il),(bp,Il)] = [(ap,bp)]
  to_pairs_step1 [_,_]             = []
  to_pairs_step1 (a@(ap,Rb):b@(bp,Lb):rest) = (ap,bp):(to_pairs_step1 $ b:rest)
  to_pairs_step1 (a@(ap,Rb):b@(bp,Il):rest) = (ap,bp):(to_pairs_step1 $ b:rest)
  to_pairs_step1 (a@(ap,Il):b@(bp,Lb):rest) = (ap,bp):(to_pairs_step1 $ b:rest)
  to_pairs_step1 (a@(ap,Il):b@(bp,Il):rest) = (ap,bp):(to_pairs_step1 $ b:rest)
  to_pairs_step1 (_:b:rest)                 = (to_pairs_step1 $ b:rest)

  interpolate :: [((Float,Float), (Float,Float))] -> [(Float, Float)] -> [(Float,Float)]
  interpolate _ []  = []
  interpolate _ [_] = row
  interpolate [(a,b)] [ar,br]
    |(a == ar)&&(b == br) = coordinates_along_the_line_f a b
    |otherwise = row

  interpolate fst@([(a,b)]) (ar:br:rowl)
    |(a == ar)&&(b == br) = (coordinates_along_the_line_f a b) ++ rowl
    |(a /= ar)||(b /= br) = ar:(interpolate fst (br:rowl))
    |otherwise = row

  interpolate fst@((a,b):rest) (ar:br:rowl)
    |(a == ar)&&(b == br) = (coordinates_along_the_line_f a b) ++ (interpolate rest rowl)
    |(a /= ar)||(b /= br) = ar:(interpolate fst (br:rowl))
    |otherwise = row

  interpolate _ _ = row
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
to_same_length:: [[(Float, Float)]] -> [[(Float, Float)]]
to_same_length []     = []
to_same_length row = map add_preffix_and_suffix $ zip3 row' mins maxs
  where
  (gminy,_) = minimumBy sortGt_by_x mins
  (gmaxy,_) = maximumBy sortGt_by_x maxs

  mins = map (minimumBy sortGt_by_x) row'
  maxs = map (maximumBy sortGt_by_x) row'

  row' = map (sortBy sortGt_by_x) $ filter (/=[]) row

  add_preffix_and_suffix :: ([(Float, Float)],(Float,Float),(Float,Float)) -> [(Float, Float)]
  add_preffix_and_suffix ([],_,_)  = []
  add_preffix_and_suffix (row,(lminy,lminz),(lmaxy,lmaxz))
    |lminy == gminy = step1 row
    |lminy > gminy = step1 $ zip pry prz ++ tail row
    |otherwise = row
    where
    step1 row
      |lmaxy == gmaxy = row
      |lmaxy < gmaxy = (init row) ++ zip sfy sfz
      |otherwise = row

    pry = [gminy .. lminy]
    st  = (abs lminz) / (fromIntegral $ length pry)
    prz = [0,st.. lminz] ++ repeat lminz

    sfy = [lmaxy..gmaxy]
    st' = (abs lmaxz) / (fromIntegral $ length sfy)
    sfz = [lmaxz, lmaxz-st' .. 0] ++ repeat 0
----------------------------------------------------------------------------------------------------







-------------------------------- end of section of processors --------------------------------------




















