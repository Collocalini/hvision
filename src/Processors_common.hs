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

 {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Processors_common (

Processor_data(..),
Processors(..),
Processor(..),
Processors',
--Processor_vs,

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
--apply_processors_v,
apply_processors_v_r,
apply_processors_v_i,
apply_processors_v_b,
apply_processors_vs_b,
apply_processors_vs_rgb8,
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
--import Processors2d
import Data.Word
--import Codec.FFmpeg.Juicy
import Control.Monad.State

import qualified Data.Matrix as DMatrix

data Processor_data = Pd Dynamic  (Dynamic -> String) --deriving (Show)
data Processors = PMRational_l [(Matrix Rational) -> (Matrix Rational)]
                | PMInt_l [(Matrix Int) -> (Matrix Int)]
                | PMWord8_l [(Matrix Word8) -> (Matrix Word8)]
                | PMWord8vs_l ((Matrix Word8) -> State (Matrix Word8) (Matrix Word8))


type Processors' = [Processor]

data Processor = PMRational ((Matrix Rational) -> (Matrix Rational))
               | PMInt ((Matrix Int) -> (Matrix Int))
               | PMWord8 ((Matrix Word8) -> (Matrix Word8))
               | PMWord8vs (((Matrix Word8) -> State (Matrix Word8) (Matrix Word8)), (Matrix Word8))
               | PMRGB8vs ((CPic.Image CPic.PixelRGB8) ->
                     State (CPic.Image CPic.PixelRGB8) (CPic.Image CPic.PixelRGB8)
                          ,(CPic.Image CPic.PixelRGB8))
   --where
   --deriving Show

{-

data Processor' where
   MkProcessor' :: Processible i o s => Proc sl i o s -> Processor'
  -- MkProcWord8' :: Proc (Matrix Word8) (Matrix Word8) () -> Processor'


data Proc sl i o s where
 --  PM' :: (i -> i) -> Proc i () ()
 --  PM'' :: (Processible i o ()) =>(i -> o) -> Proc i o ()
 --  PMvs' :: ((i -> o), s) -> Proc i o s
 --  PMvs'' :: ((i -> State s o), s) -> Proc i o s
   PMWord8'' :: ((Matrix Word8) -> (Matrix Word8)) -> Proc Proc_slot (Matrix Word8) (Matrix Word8) ()
   PMWord8vs' :: ((Matrix Word8) -> State (Matrix Word8) (Matrix Word8), (Matrix Word8)) -> Proc Proc_slot (Matrix Word8) (Matrix Word8) (Matrix Word8)


data Proc_slot = PMWord8'''|PMWord8vs'''

--data Proc_slot' where
--   PMWord8'''':: Proc_slot'
--   PMWord8vs'''':: Proc_slot'



class Processible sl i o s where
   run :: i -> Proc sl i o s -> (o, Proc sl i o s)
 --  run :: i -> (i -> i) -> o
 --  run :: i -> ((i -> State s o), s) -> o

instance Processible PMWord8''' (Matrix Word8) (Matrix Word8) () where
   run i x@(PMWord8'' p) = (p i, x)


instance Processible (Matrix Word8) (Matrix Word8) (Matrix Word8) where
   run i (PMWord8vs' (p, s)) = (\(a,st)-> (a, PMWord8vs' (p, st))) $ runState (p i) s
-}
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
apply_processors_v_b :: [(Matrix Word8) -> (Matrix Word8)] ->
                                     Matrix Word8 -> (Matrix Word8)
apply_processors_v_b [last] input = last input
apply_processors_v_b (processor:rest) input = apply_processors_v_b rest $ processor input
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
--apply_processors_vs_b :: PMWord8vs ->
--                                     Matrix Word8 -> (Matrix Word8)
--apply_processors_vs_b [(p, s)] input = p input s

--apply_processors_vs_b (processor:rest) input = apply_processors_v_b rest $ processor input
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================
apply_processors_v' :: a -> State ([PMWord8vs' a s]) a
apply_processors_v' frame = do
   ps <- get
   let psm = map unpack_proc ps
   let (p,ss) = unzip psm
   let(r,s) = step1 frame (head ss) (head p)

   put [MkPMWord8vs' (head p) s]
   return r
   where
   step1 :: a -> s -> (a -> State s a) -> (a, s)
   step1 f st proc = runState (proc f) st
--apply_processors_v ((proc, st):rest) input = proc input
--apply_processors_v ((MkProcessor2d processor):rest) input = apply_processors_v rest $ processor input
----------------------------------------------------------------------------------------------------
--}


{-
test_apply :: (Matrix Word8) -> (Matrix Word8)
test_apply f = --do
   (\(a,_) -> a) $ runState (apply_processors_v f) $ [MkProcessor' $ MkPMWord8vs' frame_difference_vs_b  (zero 1 1)]
   --return a
-}


{-- ================================================================================================
================================================================================================ --}
apply_processors_vs :: a -> Processors' ->
                            Processors' ->
                            (a -> Processor -> (a, Processor)) ->
                            (a, Processors')
apply_processors_vs a x [] _ = (a,x)
apply_processors_vs a x (p:rest) f = (\(a,ps) -> apply_processors_vs a (x ++ [ps]) rest f) $ f a p
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
apply_processors_vs_b :: Processors' -> (Matrix Word8) -> ((Matrix Word8), Processors')
apply_processors_vs_b x frame = apply_processors_vs frame [] x apply_processor_vs_b
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
apply_processors_vs_rgb8 :: Processors' -> (CPic.Image CPic.PixelRGB8) ->
                                          ((CPic.Image CPic.PixelRGB8), Processors')
apply_processors_vs_rgb8 x frame = apply_processors_vs frame [] x apply_processor_vs_rgb8
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
apply_processor_vs_b :: (Matrix Word8) -> Processor -> ((Matrix Word8), Processor)
apply_processor_vs_b frame x@(PMWord8 p) = (p frame, x)
apply_processor_vs_b frame (PMWord8vs (p,s)) = (\(a,st) -> (a, PMWord8vs (p,st) )) $
                                                                                runState (p frame) s
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
apply_processor_vs_rgb8 :: (CPic.Image CPic.PixelRGB8) -> Processor ->
                          ((CPic.Image CPic.PixelRGB8), Processor)
apply_processor_vs_rgb8 frame (PMRGB8vs (p,s)) = (\(a,st) -> (a, PMRGB8vs (p,st) )) $
                                                                                runState (p frame) s

----------------------------------------------------------------------------------------------------



--MkProcWord8'

{-do
   ps <- get
   --let psm = map (\(MkPMWord8vs' f s) -> (f,s)) ps -- map unpack_proc ps
   --let (p,ss) = unzip psm
   let (r,s) = step1 frame (head ps)

   put [MkProcessor' s]
   return r
   where
   --step1 :: a -> s -> (a -> State s a) -> (a, s)
   --step1 f st proc = runState (proc f) st
   step1 :: Processible a i s => i -> a -> (i , a)
   step1 frame p = run frame p
--apply_processors_v ((proc, st):rest) input = proc input
--apply_processors_v ((MkProcessor2d processor):rest) input = apply_processors_v rest $ processor input
----------------------------------------------------------------------------------------------------
-}


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














