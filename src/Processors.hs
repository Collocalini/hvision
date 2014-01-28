
module Processors (

identity_i,
identity_i_dyn,
identity_f,
identity_f_dyn,
derivative_i,
derivative_i_dyn,
derivative_f,
derivative_f_dyn,

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
toStringTable,
Processor_data,

) where

import Data.List
import Data.Dynamic
import qualified Data.Map as DMap

--
data Processor_data = Pd Dynamic  (Dynamic -> String) --deriving (Show)


eol_char = "\n"



{-- ================================================================================================
================================================================================================ --}
--processorDataMap:: DMap.Map ([(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)])
--                            (Int -> Int -> String ->  [(a, b)])
--processorDataMap = DMap.fromList [
        --("",""),

--   ]----]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
----------------------------------------------------------------------




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
identity_i_dyn  row = map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0::Int) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0::Int) ) )) $
                      identity_i $
                      map (\(x, y) -> ((fromDyn x 0)  , (fromDyn y 0) )) row
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
identity_f_dyn  row = map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0::Float)),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0::Float)) )) $
                      identity_f $
                      map (\(x, y) -> ((fromDyn x 0)::Float  , (fromDyn y (0::Float)) )) row
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
derivative_i_dyn  row = map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0::Int) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0::Int) ) )) $
                      derivative_i $
                      map (\(x, y) -> ((fromDyn x 0)::Int  , (fromDyn y 0)::Int )) row
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
derivative_f_dyn  row = map (\(x, y) -> (Pd (toDyn x) (show . \z -> fromDyn z (0::Float) ),
                                      Pd (toDyn y) (show . \z -> fromDyn z (0::Float) ) )) $
                      derivative_f $
                      map (\(x, y) -> ((fromDyn x 0)::Float  , (fromDyn y 0)::Float )) row
---------------------------------------------------------------------




-------------------------------- end of section of processors --------------------------------------





{-- ================================================================================================
================================================================================================ --}
apply_processors :: [( [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)] )] ->
                                     [(Dynamic, Dynamic)] -> [[ (Processor_data, Processor_data) ]]
apply_processors [] _ = []
apply_processors (processor:rest) input = (processor input):(apply_processors rest input)
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
stack_output :: [[ (Processor_data, Processor_data) ]] -> [[Processor_data]]
stack_output [] = []
stack_output (data_ : rest ) = (\(x, y) -> x:y:(step1 rest) ) $ unzip data_
    where
    step1 :: [[ (Processor_data, Processor_data) ]] -> [[Processor_data]]
    step1 [] = []
    step1 (data_ : rest ) = (\(x, y) -> y:(step1 rest)) $ unzip data_
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
toStringTable :: [[Processor_data]] -> String
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
  step1 str = (\x -> (toDyn ((read $ head x)::Int) ,toDyn ((read $ head $ tail x)::Int) ) ) $
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











