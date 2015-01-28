-----------------------------------------------------------------------------
--
-- Module      :  Processors_common
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

module Processors_common (

Processor_data(..),
Processors(..),

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
apply_processors_v,
apply_processors_v_r,
apply_processors_v_i,
apply_processors_v_b,
apply_processors_context_sensitive,
stack_output,
stack_output_each_xy,
stack_output_matrix,
--stack_output_matrix_context,
toStringTable,
toStringTable_matrix,
--matrix_rationalToString,
--imageY8ToMatrix_rational,
--imageToMatrix_rational,
) where
import Data.Dynamic
import Data.List
import Global
import Data.Matrix
import qualified Codec.Picture as CPic
import Image_loading
import Processors2d
--import Codec.FFmpeg.Juicy

data Processor_data = Pd Dynamic  (Dynamic -> String) --deriving (Show)
data Processors = PMRational [(Matrix Rational) -> (Matrix Rational)]
                | PMInt [(Matrix Int) -> (Matrix Int)]




{-- ================================================================================================
================================================================================================ --}
head_repeats _ [] = []
head_repeats row l = replicate (abs $ length row - length l) (head l) ++ l
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
apply_processors_v_r :: [(Matrix Rational) -> (Matrix Rational)] ->
                                     Matrix Rational -> (Matrix Rational)
apply_processors_v_r [last] input = last input
apply_processors_v_r (processor:rest) input = apply_processors_v_r rest $ processor input
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
apply_processors_v_i :: [(Matrix Int) -> (Matrix Int)] ->
                                     Matrix Int -> (Matrix Int)
apply_processors_v_i [last] input = last input
apply_processors_v_i (processor:rest) input = apply_processors_v_i rest $ processor input
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
apply_processors_v_b :: [(Matrix Char) -> (Matrix Char)] ->
                                     Matrix Char -> (Matrix Char)
apply_processors_v_b [last] input = last input
apply_processors_v_b (processor:rest) input = apply_processors_v_b rest $ processor input
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
apply_processors_v :: Matrix' a => Processors -> a -> a
apply_processors_v (PMRational proc) input = input
--apply_processors_v ((MkProcessor2d processor):rest) input = apply_processors_v rest $ processor input
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
apply_processors_context_sensitive ::
                             [( [[(Dynamic, Dynamic)]] -> [[(Processor_data, Processor_data)]] )] ->
                                [[(Dynamic, Dynamic)]] -> [[[ (Processor_data, Processor_data) ]]]
apply_processors_context_sensitive [] _ = []
apply_processors_context_sensitive p i = --[apply_processors [identity_f_dyn] $ head i]
                                         --step1 p i
                                         rearrange $ step1 p i
  where

  step1 :: [( [[(Dynamic, Dynamic)]] -> [[(Processor_data, Processor_data)]] )] ->
                                [[(Dynamic, Dynamic)]] -> [[[ (Processor_data, Processor_data) ]]]
  step1 [] _ = []
  step1 (processor:rest) input = (processor input) : (step1 rest input)

  rearrange :: [[[ (Processor_data, Processor_data) ]]] -> [[[ (Processor_data, Processor_data) ]]]
  rearrange [] = []
  rearrange d = run_slices ([], d)

  slice :: [[[ (Processor_data, Processor_data) ]]] ->
           [[[ (Processor_data, Processor_data) ]]] ->
            [[ (Processor_data, Processor_data) ]] ->
           ([[ (Processor_data, Processor_data) ]], [[[ (Processor_data, Processor_data) ]]])
  slice []        []                       [] = ([], [])
  slice []        []                       hs = (reverse hs, [])
  slice r         []                       hs = (reverse hs, reverse r)
  slice []       ((h:hrest):thisIteration) [] = slice ([hrest])
                                                      (thisIteration)
                                                      ([h])
  slice []       ((h:[]):thisIteration)    [] = slice ([])
                                                      (thisIteration)
                                                      ([h])
  slice []       (([]):thisIteration)      [] = slice ([])
                                                      (thisIteration)
                                                      ([])
  slice remained ((h:hrest):thisIteration) hs = slice (hrest :remained)
                                                      (thisIteration)
                                                      (h:hs)
  slice remained ((h:[]):thisIteration)    hs = slice (remained)
                                                      (thisIteration)
                                                      (h:hs)
  slice remained (([]):thisIteration)      hs = slice (remained)
                                                      (thisIteration)
                                                      (hs)

  run_slices ::
              ([[ (Processor_data, Processor_data) ]], [[[ (Processor_data, Processor_data) ]]]) ->
              [[[ (Processor_data, Processor_data) ]]] -- ->
  run_slices (_, [])         = []
  run_slices ([], remained)  = run_slices $ slice [] remained []
  run_slices (row, remained) = row:(run_slices $ slice [] remained [])
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






{-- ================================================================================================
================================================================================================ --}
--imagePToMatrix_rational :: JuicyPixelFormat p => CPic.Image p -> (Matrix Rational)
--imagePToMatrix_rational (CPic.Image image) = imageY8ToMatrix_rational image

----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================
imageToMatrix :: Image' a => Matrix' b => a -> b
imageToMatrix img = imageY8ToMatrix_rational $ to_grayscale8 img
----------------------------------------------------------------------------------------------------
--}

{-- ================================================================================================
================================================================================================
imageY8ToMatrix_rational :: CPic.Image CPic.Pixel8 -> (Matrix Rational)
imageY8ToMatrix_rational image@(CPic.Image {CPic.imageWidth  = width
                                           ,CPic.imageHeight = height}) =

   matrix width height $ \(x,y) -> fromIntegral $ CPic.pixelAt image (x-1) (y-1)

   where
   --cords = [(x,y) | x <- [0..width-1], y <- [0..height-1]]



----------------------------------------------------------------------------------------------------

--}

{-- ================================================================================================
================================================================================================ --}
imageToMatrix_rational :: Image' a => a -> (Matrix Rational)
imageToMatrix_rational img = imageY8ToMatrix_fractional $ to_grayscale8 img
----------------------------------------------------------------------------------------------------














