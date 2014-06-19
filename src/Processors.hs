
module Processors (

identity_i,
identity_i_dyn,
identity_f,
identity_f_dyn,
derivative_i,
derivative_i_dyn,
derivative_f,
derivative_f_dyn,
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

stringToIntList,
stringToIntList_dyn,
intListToString,
stringToFloatList,
floatListToString,

stringToIntList_mn,
stringToIntList_mn_dyn,
stringToFloatList_mn,
stringToFloatList_mn_dyn,
intListToString_2to3,
floatListToString_2to3,
apply_processors,
stack_output,
stack_output_matrix,
toStringTable,
toStringTable_matrix,
Processor_data,

) where

import Data.List
import Data.Dynamic
import qualified Data.Map as DMap

--
data Processor_data = Pd Dynamic  (Dynamic -> String) --deriving (Show)

data MarkExtremums = Max|Min|Both deriving (Eq)

eol_char = "\n"


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
identity_f_dyn  row = map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float)),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float)) )) $
                      identity_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float , (fromDyn y (0:: Float)) )) row
---------------------------------


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
derivative_f_dyn  row = map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      derivative_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float , (fromDyn y 0):: Float )) row
---------------------------------------------------------------------








{-- ================================================================================================
================================================================================================ --}
derivative_type1_f :: [(Float, Float)] -> [(Float, Float)]
derivative_type1_f  [] = []
derivative_type1_f  row@((x_prev, _):_) = (x_prev, 0):(step1 row)
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
derivative_type1_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
derivative_type1_f_dyn  row = map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      derivative_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float , (fromDyn y 0):: Float )) row
---------------------------------------------------------------------













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


{-- ================================================================================================
================================================================================================ --}
processor_x_2_f :: [(Float, Float)] -> [(Float, Float)]
processor_x_2_f row = --head_repeats row $
                      processor_x_n 2 $ mark_extremums Max row
  where

----------------------------------------------------------------------------------------------------

{-- ================================================================================================
================================================================================================ --}
processor_x_2_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
processor_x_2_f_dyn  row =map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      processor_x_2_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
processor_x_2_2_f :: [(Float, Float)] -> [(Float, Float)]
processor_x_2_2_f row =  --head_repeats row $
                         processor_x_n 2 $
                         mark_extremums Max $
                         processor_x_n 2 $ mark_extremums Max row
----------------------------------------------------------------------------------------------------

{-- ================================================================================================
================================================================================================ --}
processor_x_2_2_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
processor_x_2_2_f_dyn  row =map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      processor_x_2_2_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
----------------------------------------------------------------------------------------------------


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

{-- ================================================================================================
================================================================================================ --}
processor_x_2_3_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
processor_x_2_3_f_dyn  row =map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      processor_x_2_3_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
----------------------------------------------------------------------------------------------------






{-- ================================================================================================
================================================================================================ --}
processor_xm_2_f :: [(Float, Float)] -> [(Float, Float)]
processor_xm_2_f row =  --head_repeats row $
                        processor_x_n 2 $ mark_extremums Min row
----------------------------------------------------------------------------------------------------

{-- ================================================================================================
================================================================================================ --}
processor_xm_2_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
processor_xm_2_f_dyn  row =map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      processor_xm_2_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
processor_xm_2_2_f :: [(Float, Float)] -> [(Float, Float)]
processor_xm_2_2_f row = --head_repeats row $
                         processor_x_n 2 $
                         mark_extremums Min $
                         processor_x_n 2 $ mark_extremums Min row
----------------------------------------------------------------------------------------------------

{-- ================================================================================================
================================================================================================ --}
processor_xm_2_2_f_dyn :: [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)]
processor_xm_2_2_f_dyn  row =map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0:: Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0:: Float) ) )) $
                      processor_xm_2_2_f $
                      map (\(x, y) -> ((fromDyn x 0):: Float  , (fromDyn y 0):: Float )) row
----------------------------------------------------------------------------------------------------


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
    step2Both (_:_:[]) = []
    step2Both (prev@(x_prev, y_prev):curr@(x_curr, y_curr):next@(x_next, y_next):rest)
        |up y_prev y_curr y_next = curr:(step2Both $ curr:next:rest)
        |down y_prev y_curr y_next = curr:(step2Both $ curr:next:rest)
        |otherwise = step2Both $ curr:next:rest

    step2max :: [(Float, Float)] -> [(Float, Float)]
    step2max [] = []
    step2max (_:_:[]) = []
    step2max (prev@(x_prev, y_prev):curr@(x_curr, y_curr):next@(x_next, y_next):rest)
        |up y_prev y_curr y_next = curr:(step2max $ curr:next:rest)
       -- |down y_prev y_curr y_next = curr:(step2max $ curr:next:rest)
        |otherwise = step2max $ curr:next:rest

    step2min :: [(Float, Float)] -> [(Float, Float)]
    step2min [] = []
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







-------------------------------- end of section of processors --------------------------------------

{-- ================================================================================================
================================================================================================ --}
head_repeats _ [] = []
head_repeats row l = replicate (abs $ length row - length l) (head l) ++ l
----------------------------------------------------------------------------------------------------

{-- ================================================================================================
================================================================================================ --}
sortLt_by_y (_, ly) (_, ry)
  | ly < ry = GT
  | ly > ry = LT
  | ly == ry = EQ
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
sortGt_by_x (lx, _) (rx, _)
  | lx < rx = LT
  | lx > rx = GT
  | lx == rx = EQ
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
apply_processors :: [( [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)] )] ->
                                     [(Dynamic, Dynamic)] -> [[ (Processor_data, Processor_data) ]]
apply_processors [] _ = []
apply_processors (processor:rest) input = (processor input):(apply_processors rest input)
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
space_out :: [(Float, Float)]  -> [(Float, Float)] -> [(Float, Float)]
space_out [] _  = []
space_out (row@(rx,_):row_rest) []
   |otherwise = (rx, 0):(space_out row_rest [] )
space_out (row@(rx,_):row_rest) (extrs@(ex,_):[])
   |otherwise = (rx, 0):(space_out row_rest [])
space_out (row@(rx,_):row_rest) (extrs@(ex,_):extrs_rest)
   |(rx <= ex) && (fst (head row_rest) > ex) = extrs:(space_out row_rest extrs_rest )
   |otherwise = (rx, 0):(space_out row_rest (extrs:extrs_rest) )
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
stack_output :: [[ (Processor_data, Processor_data) ]] -> [[Processor_data]]
stack_output [] = []
stack_output (data_ : rest ) = stack_output_each_xy (data_ : rest )  -- !!!!!WARNING DIRTY HACK!!!!!
                               -- (\(x, y) -> x:y:(step1 rest) ) $ unzip data_
    where
    step1 :: [[ (Processor_data, Processor_data) ]] -> [[Processor_data]]
    step1 [] = []
    step1 (data_ : rest ) = (\(x, y) -> y:(step1 rest)) $ unzip data_
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
stack_output_each_xy :: [[ (Processor_data, Processor_data) ]] -> [[Processor_data]]
stack_output_each_xy [] = []
stack_output_each_xy (data_ : rest ) = (\(x, y) -> x:y:(stack_output_each_xy rest) ) $ unzip data_
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
stack_output_matrix :: [[[ (Processor_data, Processor_data) ]]] ->
                       [[[(Processor_data, Processor_data, Processor_data)]]]
stack_output_matrix [] = []
stack_output_matrix input = step1 0 input
                               --(step1 data_) ++ (stack_output_matrix rest)
                                --(\(x, y) -> x:y:(step1 rest) ) $ unzip data_
    where
    step1 :: Int -> [[[ (Processor_data, Processor_data) ]]] ->
                    [[[(Processor_data, Processor_data, Processor_data)]]]
    step1 _ [] = []
    step1 i (d_ : r ) = (step2 i d_) : (step1 (i+1) r)
      where
      step2 :: Int -> [[ (Processor_data, Processor_data) ]] ->
               [[(Processor_data, Processor_data, Processor_data)]]
      step2 _ [] = []
      step2 i' (data_ : rest ) = (map (step3 i'') data_):(step2 i' rest)
        where
          --i'= replicate (length data_) $ Pd (toDyn i) (show . \z -> fromDyn z (0:: Int))
          i''= Pd (toDyn i') (show . \z -> fromDyn z (0:: Int))

      step3 :: Processor_data ->
               (Processor_data, Processor_data) ->
               (Processor_data, Processor_data, Processor_data)
      step3 y (x,z) = (x,y,z)
----------------------------------------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
toStringTable :: [[Processor_data]] -> String
toStringTable [] = []
toStringTable [[]] = []
toStringTable data_@(left: rest) = unlines $ step_join
                                (map (\(Pd d f) -> f d) left)
                                $ step2 rest
   where
      step_join :: [String] -> [String] -> [String]
      step_join []          (y:resty) = ("" ++ " " ++   y) :(step_join [] resty)
      step_join (x:restx)   []        = (x  ++ " " ++  "") :(step_join restx [])
      step_join []          []        = []
      step_join (x:restx)   (y:resty) = (x  ++ " " ++   y) :(step_join restx resty)

      step2 :: [[Processor_data]] -> [String]
      step2 [] = []
      step2 (left : rest) = step_join (map (\(Pd d f) -> f d) left) $ step2 rest
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
toStringTable_matrix :: [[[(Processor_data, Processor_data, Processor_data)]]] -> String
toStringTable_matrix [] = []
toStringTable_matrix [[]] = []
toStringTable_matrix data_ = allFramess data_
   where
   toStrings :: (Processor_data, Processor_data, Processor_data) ->
                (String, String, String)
   toStrings (x,y,z) = ( (\(Pd d f) -> f d) x, (\(Pd d f) -> f d) y, (\(Pd d f) -> f d) z)

   toSingleLine :: (String, String, String) -> String
   toSingleLine (x,y,z) = unwords [x, y, z]

   {--toSingleProcessor ::  [String] -> String
   toSingleProcessor s = unlines s

   toSingleY ::  [String] -> String
   toSingleY s = unlines s--}

   processor :: [(Processor_data, Processor_data, Processor_data)] -> String
   processor p = unlines $ map (toSingleLine.toStrings) p

   allProcessors :: [[(Processor_data, Processor_data, Processor_data)]] -> String
   allProcessors  pp = concat $ map processor pp

   allFramess :: [[[(Processor_data, Processor_data, Processor_data)]]] -> String
   allFramess f = --unlines $ map ((eol_char ++) . allProcessors) f
                  unlines $ map (allProcessors) f
----------------------------------------------------------------------------------------------------





------------------read columns 1,2 of (1..inf)------------------------------------------------------
{-- ================================================================================================
================================================================================================ --}
stringToIntList :: String -> [(Int, Int)]
stringToIntList str =  map step1 $ lines str
  where
  step1 :: String -> (Int, Int)
  step1 str = (\x -> (read $ head x , read $ head $ tail x) ) $ words str
{--    |
       |
       |
       |
       V  --}
stringToIntList_dyn :: String -> [(Dynamic, Dynamic)]
stringToIntList_dyn str =  map step1 $ lines str
  where
  step1 :: String -> (Dynamic, Dynamic)
  step1 str = (\x -> (toDyn ((read $ head x):: Int) ,toDyn ((read $ head $ tail x):: Int) ) ) $
                                                                                          words str
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
stringToFloatList :: String -> [(Float, Float)]
stringToFloatList str =  map step1 $ lines str
  where
  step1 :: String -> (Float, Float)
  step1 str = (\x -> (read $ head x , read $ head $ tail x) ) $ words str
----------------------------------------------------------------------------------------------------

------------------end of ---- read columns 1,2 of (1..inf)------------------------------------------







------------------read columns m,n of (1..inf)-----------------------------------------------------
{-- ================================================================================================
================================================================================================ --}
stringToIntList_mn :: Int -> Int -> String ->  [(Int, Int)]
stringToIntList_mn m n str =  map (step1 m n) $ lines str
  where
  step1 :: Int -> Int -> String ->   (Int, Int)
  step1 m n str = (\x -> (read $ head $ drop (m-1) x , read $ head $ drop (n-1) x) ) $ words str
{--    |
       |
       |
       |
       V  --}
stringToIntList_mn_dyn :: Int -> Int -> String -> [(Dynamic, Dynamic)]
stringToIntList_mn_dyn m n str =  map (\(x, y) -> ((toDyn x), (toDyn y)) ) $
                                                                          stringToIntList_mn m n str
----------------------------------------------------------------------------------------------------

{-- ================================================================================================
================================================================================================ --}
stringToFloatList_mn :: Int -> Int -> String ->  [(Float, Float)]
stringToFloatList_mn m n str =  map (step1 m n) $ lines str
  where
  step1 :: Int -> Int -> String ->  (Float, Float)
  step1 m n str = (\x -> (read $ head $ drop (m-1) x , read $ head $ drop (n-1) x) ) $ words str
{--    |
       |
       |
       |
       V  --}
stringToFloatList_mn_dyn :: Int -> Int -> String -> [(Dynamic, Dynamic)]
stringToFloatList_mn_dyn m n str =  map (\(x, y) -> ((toDyn x), (toDyn y)) ) $
                                                                       stringToFloatList_mn m n str
----------------------------------------------------------------------------------------------------

------------------end of ---- read columns m,n of (1..inf)------------------------------------------









------------------ put 2 columns back as string ----------------------------------------------------
{-- ================================================================================================
================================================================================================ --}
intListToString :: [(Int, Int)] -> String
intListToString x = concat $ map (\(x,y) -> (show x) ++ " " ++ (show y) ++ eol_char ) x
---------------------------------


{-- ================================================================================================
================================================================================================ --}
floatListToString :: [(Float, Float)] -> String
floatListToString x = concat $ map (\(x,y) -> (show x) ++ " " ++ (show y) ++ eol_char ) x
---------------------------------

------------------ end of ------ put 2 columns back as string -------------------------------------







------------------ put 2 graphs as 3 columns ----------------------------------------------------
step2to3 :: (a, b) -> (c, d) -> (a, b, d)
step2to3 (a, b) (c, d) = (a, b, d)

{-- ================================================================================================
================================================================================================ --}
intListToString_2to3 :: [(Int, Int)] -> [(Int, Int)] -> String
intListToString_2to3 y y1 = concat $ map
        (\(x,w,z) -> (show x) ++ " " ++ (show w) ++ " " ++ (show z) ++ eol_char ) $ zipWith step2to3 y y1

---------------------------------


{-- ================================================================================================
================================================================================================ --}
floatListToString_2to3 :: [(Float, Float)] -> [(Float, Float)] -> String
floatListToString_2to3 y y1 = concat $ map
        (\(x,w,z) -> (show x) ++ " " ++ (show w) ++ " " ++ (show z) ++ eol_char ) $ zipWith step2to3 y y1
---------------------------------

------------------ end of ------ put 2 graphs as 3 columns -------------------------------------





------------------ put n graphs as m columns ----------------------------------------------------

toString_zip :: [String] -> [String] -> [String]
toString_zip x y = map (\(x,y) -> x ++ " " ++ " " ++ y ) $ zip x y


xy2string_ii :: [(Int, Int)] -> ([String] , [String])
xy2string_ii x = unzip $ map (\(x,y) -> (show x, show y) ) x

xy2string_if :: [(Int, Float)] -> ([String] , [String])
xy2string_if x = unzip $ map (\(x,y) -> (show x, show y) ) x

xy2string_ff :: [(Float, Float)] -> ([String] , [String])
xy2string_ff x = unzip $ map (\(x,y) -> (show x, show y) ) x

xy2string_fi :: [(Float, Int)] -> ([String] , [String])
xy2string_fi x = unzip $ map (\(x,y) -> (show x, show y) ) x

------------------ end of ------ put n graphs as m columns -------------------------------------











