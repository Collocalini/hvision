-----------------------------------------------------------------------------
--
-- Module      :  Recognize_demanded_processors
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

module Recognize_demanded_processors (
recognizeDemanded_processors,
recognizeDemanded_processors_frame_context_sensitive,
recognizeDemanded_processors_v,
recognizeDemanded_processors_v',

identity_i_processor',
identity_f_processor',
derivative_f_processor',
derivative_i_processor',
max_derivative_in_range_xy_f_processor',
min_derivative_in_range_xy_f_processor',
distance_between_extremums_f_processor',
extremums_f_processor',
processor_x_2_f_processor',
processor_x_2_2_f_processor',
processor_x_2_3_f_processor',

processor_xm_2_f_processor',
processor_xm_2_2_f_processor',
processor_xm_2_3_f_processor',

frame_difference_sequence_processor',
histogram_y_per_pixel_multiple_rows_f_processor',
histogram_y_per_pixel_multiple_rows_dft_f_processor',

ad_hock_f_processor',
) where

import Processors_common
import Processors
import Processors2d
import Data.Dynamic
import Data.Matrix
import Data.List
import Data.Word
import qualified Codec.Picture as CPic

recognizeDemanded_processors :: [String] ->
                           [( [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)] )]
recognizeDemanded_processors [] = []
recognizeDemanded_processors proc = step2 proc
    where
    step2 :: [String] -> [( [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)] )]
    step2 [] = []
    step2 (proc:rest)
        |identity_i_processor' proc = identity_i_dyn:(step2 rest)
        |identity_f_processor' proc = identity_f_dyn:(step2 rest)
        |derivative_f_processor' proc = derivative_f_dyn:(step2 rest)
        |derivative_i_processor' proc = derivative_i_dyn:(step2 rest)
        |max_derivative_in_range_xy_f_processor' proc = max_derivative_in_range_xy_f_dyn:
                                                                                    (step2 rest)
        |min_derivative_in_range_xy_f_processor' proc = min_derivative_in_range_xy_f_dyn:
                                                                                    (step2 rest)
        |distance_between_extremums_f_processor' proc = distance_between_extremums_f_dyn:
                                                                                    (step2 rest)
        |extremums_f_processor' proc = extremums_f_dyn:(step2 rest)
        |processor_x_2_f_processor' proc = processor_x_2_f_dyn:(step2 rest)
        |processor_x_2_2_f_processor' proc = processor_x_2_2_f_dyn:(step2 rest)
        |processor_x_2_3_f_processor' proc = processor_x_2_3_f_dyn:(step2 rest)
        |processor_xm_2_f_processor' proc = processor_xm_2_f_dyn:(step2 rest)
        |processor_xm_2_2_f_processor' proc = processor_xm_2_2_f_dyn:(step2 rest)
        |processor_xm_2_3_f_processor' proc = processor_xm_2_3_f_dyn:(step2 rest)
       -- |ad_hock_f_processor' proc = filter_range_f_dyn:(step2 rest)
        |otherwise = step2 rest


recognizeDemanded_processors_frame_context_sensitive :: [String] ->
                       [( [[(Dynamic, Dynamic)]] -> [[(Processor_data, Processor_data)]] )]
recognizeDemanded_processors_frame_context_sensitive [] = []
recognizeDemanded_processors_frame_context_sensitive proc = step2 proc
    where
    step2 :: [String] -> [( [[(Dynamic, Dynamic)]] -> [[(Processor_data, Processor_data)]] )]
    step2 [] = []
    step2 (proc:rest)
        |frame_difference_sequence_processor' proc = frame_difference_sequence_f_dyn:
                                                                                    (step2 rest)
        |histogram_y_per_pixel_multiple_rows_f_processor' proc =
                                          histogram_y_per_pixel_multiple_rows_f_dyn:(step2 rest)
        |histogram_y_per_pixel_multiple_rows_dft_f_processor' proc =
                                      histogram_y_per_pixel_multiple_rows_dft_f_dyn:(step2 rest)
        |ad_hock_f_processor' proc = histogram_ad_hock_f_dyn:(step2 rest)
        |otherwise = step2 rest




recognizeDemanded_processors_v_r :: [String] -> [(Matrix Rational) -> (Matrix Rational)]
recognizeDemanded_processors_v_r [] = []
recognizeDemanded_processors_v_r (proc:rest)
  |identity_v_r_processor proc = (identity_v_r):(recognizeDemanded_processors_v_r rest)
  |otherwise = recognizeDemanded_processors_v_r rest


recognizeDemanded_processors_v_i :: [String] -> [(Matrix Int) -> (Matrix Int)]
recognizeDemanded_processors_v_i [] = []
recognizeDemanded_processors_v_i (proc:rest)
  |identity_v_i_processor proc = (identity_v_i):(recognizeDemanded_processors_v_i rest)
  |otherwise = recognizeDemanded_processors_v_i rest

recognizeDemanded_processors_v_b :: [String] -> [(Matrix Word8) -> (Matrix Word8)]
recognizeDemanded_processors_v_b [] = []
recognizeDemanded_processors_v_b (proc:rest)
  |identity_v_b_processor proc = (identity_v_b):(recognizeDemanded_processors_v_b rest)
  |otherwise = recognizeDemanded_processors_v_b rest



recognizeDemanded_processors_v :: [String] -> Maybe Processors
recognizeDemanded_processors_v [] = Nothing
recognizeDemanded_processors_v p
  |not $ null pmr  = Just $ PMRational_l pmr
  |not $ null pmi = Just $ PMInt_l pmi
  |not $ null pmb = Just $ PMWord8_l pmb
  |otherwise = Nothing
  where
    pmr = recognizeDemanded_processors_v_r p
    pmi = recognizeDemanded_processors_v_i p
    pmb = recognizeDemanded_processors_v_b p



recognizeDemanded_processors_v' :: [String] -> Maybe Processors'
recognizeDemanded_processors_v' [] = Nothing
recognizeDemanded_processors_v' pp = Just $ step1 pp
  where
    step1 :: [String] -> Processors'
    step1 [] = []
    step1 (p:rest)
      |identity_v_b_processor p = (PMWord8 identity_v_b):(step1 rest)
      |frame_difference_v_b_processor p = (PMWord8vs (frame_difference_vs_b, zero 0 0 )):(step1 rest)
      |frame_difference_v_gsRGB8_processor p = (PMRGB8vs (frame_difference_vs_gsrgb8, img)):(step1 rest)
      |otherwise = step1 rest
      where
      img = CPic.generateImage (\x y -> CPic.PixelRGB8 0 0 0) 0 0


--p2dpack


identity_i_processor' :: String -> Bool
identity_i_processor' str
   |"identity_i" == str = True
   |otherwise = False


identity_f_processor' :: String -> Bool
identity_f_processor' str
   |"identity_f" == str = True
   |otherwise = False


identity_v_r_processor :: String -> Bool
identity_v_r_processor str
   |"identity_v_r" == str = True
   |otherwise = False


identity_v_i_processor :: String -> Bool
identity_v_i_processor str
   |"identity_v_i" == str = True
   |otherwise = False


identity_v_b_processor :: String -> Bool
identity_v_b_processor str
   |"identity_v_b" == str = True
   |otherwise = False

derivative_f_processor' :: String -> Bool
derivative_f_processor' str
   |"derivative_f" == str = True
   |otherwise = False


derivative_i_processor' :: String -> Bool
derivative_i_processor' str
   |"derivative_i" == str = True
   |otherwise = False


max_derivative_in_range_xy_f_processor' :: String -> Bool
max_derivative_in_range_xy_f_processor' str
   |"max_derivative_in_range_xy_f" == str = True
   |otherwise = False


min_derivative_in_range_xy_f_processor' :: String -> Bool
min_derivative_in_range_xy_f_processor' str
   |"min_derivative_in_range_xy_f" == str = True
   |otherwise = False


distance_between_extremums_f_processor' :: String -> Bool
distance_between_extremums_f_processor' str
   |"distance_between_extremums_f" == str = True
   |otherwise = False

extremums_f_processor' :: String -> Bool
extremums_f_processor' str
   |"extremums_f" == str = True
   |otherwise = False


processor_x_2_f_processor' :: String -> Bool
processor_x_2_f_processor' str
   |"processor_x_2_f" == str = True
   |otherwise = False


processor_x_2_2_f_processor' :: String -> Bool
processor_x_2_2_f_processor' str
   |"processor_x_2_2_f" == str = True
   |otherwise = False

processor_x_2_3_f_processor' :: String -> Bool
processor_x_2_3_f_processor' str
   |"processor_x_2_3_f" == str = True
   |otherwise = False


processor_xm_2_f_processor' :: String -> Bool
processor_xm_2_f_processor' str
   |"processor_xm_2_f" == str = True
   |otherwise = False


processor_xm_2_2_f_processor' :: String -> Bool
processor_xm_2_2_f_processor' str
   |"processor_xm_2_2_f" == str = True
   |otherwise = False

processor_xm_2_3_f_processor' :: String -> Bool
processor_xm_2_3_f_processor' str
   |"processor_xm_2_3_f" == str = True
   |otherwise = False


frame_difference_sequence_processor' :: String -> Bool
frame_difference_sequence_processor' str
   |"frame_difference_sequence_f" == str = True
   |otherwise = False

histogram_y_per_pixel_multiple_rows_f_processor' :: String -> Bool
histogram_y_per_pixel_multiple_rows_f_processor' str
   |"histogram_y_per_pixel_multiple_rows_f" == str = True
   |otherwise = False


histogram_y_per_pixel_multiple_rows_dft_f_processor' :: String -> Bool
histogram_y_per_pixel_multiple_rows_dft_f_processor' str
   |"histogram_y_per_pixel_multiple_rows_dft_f" == str = True
   |otherwise = False


ad_hock_f_processor' :: String -> Bool
ad_hock_f_processor' str
   |"ad_hock_f" == str = True
   |otherwise = False


frame_difference_v_b_processor :: String -> Bool
frame_difference_v_b_processor str
   |"frame_difference_v_b" == str = True
   |otherwise = False

frame_difference_v_gsRGB8_processor :: String -> Bool
frame_difference_v_gsRGB8_processor str
   |"frame_difference_v_gsRGB8" == str = True
   |otherwise = False






