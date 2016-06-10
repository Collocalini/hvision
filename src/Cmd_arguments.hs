-----------------------------------------------------------------------------
--
-- Module      :  Cmd_arguments
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

module Cmd_arguments (
argument_data_file ,
argument_single_data_file,
argument_multipage_data_file ,
argument_data_from_stdin,
argument_gnuplot_file ,
argument_range_of_files ,
argument_test,
argument_shakingAbacusTest,
argument_data_bypass_mode ,
argument_data_process,
argument_use_columns,
argument_repeat_frames_of_output,
argument_matrix_stacking,
argument_from_image_to_data_file,
argument_coords,

default_data_file,
default_single_data_file,
default_multipage_data_file,
default_data_from_stdin,
default_gnuplot_file,
default_range_of_files,
default_test,
default_shakingAbacusTest,
default_data_bypass_mode,
default_data_process,
default_use_columns,
default_repeat_frames_of_output,
default_matrix_stacking,
default_from_image_to_data_file,
default_coords,
flags ,

options,

tag_DMap,

list_arguments,

get_demanded_processors,
get_demanded_columns,
get_demanded_coords,
InputArguments(..),
inputArgs,
) where

import qualified Data.Map as DMap
import Data.Dynamic
import Processors_common
import Processors
import Processors2d
import Recognize_demanded_processors
import Data.Matrix

data InputArguments = InputArguments {
      data_file :: Maybe FilePath
     ,output_video_file :: Maybe FilePath
     ,single_data_file :: Maybe Bool
     ,multipage_data_file :: Maybe Int
     ,data_from_stdin :: Maybe Int
     ,gnuplot_file :: Maybe FilePath
     ,range_of_files :: Maybe [Int]
     ,test :: Maybe Bool
     ,shakingAbacusTest :: Maybe Bool
     ,data_bypass_mode :: Maybe Bool
     ,data_process :: Maybe [( [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)] )]
     ,data_process_cs :: Maybe [( [[(Dynamic, Dynamic)]] -> [[(Processor_data, Processor_data)]] )]
     --,data_process_v :: Maybe [(Matrix Rational) -> (Matrix Rational)]
     ,data_process_v :: Maybe Processors
     ,data_process_vs :: Maybe Processors'
     ,use_columns :: Maybe (Int, Int)
     ,repeat_frames_of_output :: Maybe Int
     ,matrix_stacking :: Maybe Bool
     ,from_image_to_data_file :: Maybe FilePath
     ,coords :: Maybe ((Int, Int),(Int, Int))
     }


{-- ================================================================================================
================================================================================================ --}
inputArgs :: DMap.Map String String -> InputArguments
inputArgs tm = InputArguments {
   data_file = data_file'
  ,output_video_file = output_video_file'
  ,single_data_file = single_data_file'
  ,multipage_data_file = multipage_data_file'
  ,data_from_stdin = data_from_stdin'
  ,gnuplot_file = gnuplot_file'
  ,range_of_files = range_of_files'
  ,test = test'
  ,shakingAbacusTest = shakingAbacusTest'
  ,data_bypass_mode = data_bypass_mode'
  ,data_process = data_process'
  ,data_process_cs = data_process_cs'
  ,data_process_v = data_process_v'
  ,data_process_vs = data_process_vs'
  ,use_columns = use_columns'
  ,repeat_frames_of_output = repeat_frames_of_output'
  ,matrix_stacking = matrix_stacking'
  ,from_image_to_data_file = from_image_to_data_file'
  ,coords = coords'
  }
  where
  data_file'
    |s/= default_data_file = Just s
    |s== "" = Nothing
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_data_file argument_data_file tm)

  output_video_file'
    |s/= default_data_file = Just s
    |s== "" = Nothing
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_output_video_file argument_output_video_file tm)

  single_data_file'
    |s == "True" = Just True
    |s == "False" = Just False
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_single_data_file argument_single_data_file tm)

  multipage_data_file'
    |s/= default_multipage_data_file = Just $ read s
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_multipage_data_file argument_multipage_data_file tm)

  data_from_stdin'
    |s/= default_data_from_stdin = Just $ read s
    |s== "" = Nothing
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_data_from_stdin argument_data_from_stdin tm)

  gnuplot_file'
    |s/= default_gnuplot_file = Just s
    |s== "" = Nothing
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_gnuplot_file argument_gnuplot_file tm)

  range_of_files'
    |s/= default_range_of_files = Just $ read s
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_range_of_files argument_range_of_files tm)

  test'
    |s == "True" = Just True
    |s == "False" = Just False
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_test argument_test tm)

  shakingAbacusTest'
    |s == "True" = Just True
    |s == "False" = Just False
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_shakingAbacusTest argument_shakingAbacusTest tm)

  data_bypass_mode'
    |s == "True" = Just True
    |s == "False" = Just False
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_data_bypass_mode argument_data_bypass_mode tm)

  data_process'
    |s/= default_data_process = Just $ recognizeDemanded_processors $ get_demanded_processors s
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_data_process argument_data_process tm)

  data_process_cs'
    |s/= default_data_process = Just $ recognizeDemanded_processors_frame_context_sensitive
                                 $ get_demanded_processors s
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_data_process argument_data_process tm)


  data_process_v'
    |s/= default_data_process = recognizeDemanded_processors_v $ get_demanded_processors s
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_data_process argument_data_process tm)

  data_process_vs'
    |s/= default_data_process = recognizeDemanded_processors_v' $ get_demanded_processors s
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_data_process argument_data_process tm)



  use_columns'
    |s/= "" = Just $ get_demanded_columns s  -- empty "" is not a mistake
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_use_columns argument_use_columns tm)

  repeat_frames_of_output'
    |s/= "" = Just $ read s  -- empty "" is not a mistake
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_repeat_frames_of_output argument_repeat_frames_of_output tm)

  matrix_stacking'
    |s == "True" = Just True
    |s == "False" = Just False
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_matrix_stacking argument_matrix_stacking tm)

  from_image_to_data_file'
    |s/= default_from_image_to_data_file = Just s
    |s== "" = Nothing
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_from_image_to_data_file argument_from_image_to_data_file tm)

  coords'
    |s/= default_coords = Just $ get_demanded_coords s
    |s== "" = Nothing
    |otherwise = Nothing
    where
    s = (DMap.findWithDefault default_coords argument_coords tm)




argument_data_file = "data-file"
argument_output_video_file = "output-video-file"
argument_single_data_file = "single-data-file"
argument_multipage_data_file = "multipage-data-file"
argument_data_from_stdin = "data-from-stdin"
argument_gnuplot_file = "gnuplot-file"
argument_range_of_files = "range-of-files" -- if range [5..10] then read data5, data6, ... data10
argument_test = "test"
argument_shakingAbacusTest = "shaking-abacus-test"
argument_data_bypass_mode = "data-bypass-mode"
argument_data_process = "data-process"
argument_use_columns = "use-columns"
argument_repeat_frames_of_output = "repeat-frames-of-output"
argument_matrix_stacking = "matrix-stacking"
argument_from_image_to_data_file = "from-image-to-data-file"
argument_coords = "coords"


default_data_file = ""
default_output_video_file = ""
default_single_data_file = "false"
default_multipage_data_file = "-"
default_gnuplot_file = ""
default_range_of_files = "" -- if range [5..10] then read data5, data6, ... data10
default_test = "false"
default_shakingAbacusTest = "false"
default_data_bypass_mode = "false"
default_data_process = "-"
default_use_columns = "1:2"
default_data_from_stdin = "-"
default_repeat_frames_of_output = "1"
default_matrix_stacking = "false"
default_from_image_to_data_file = "-"
default_coords = "-"




flags = [
         argument_test,
         argument_shakingAbacusTest,
         argument_data_bypass_mode,
         argument_single_data_file,
         argument_matrix_stacking
        ]

options =  [
            argument_data_file ,
            argument_output_video_file ,
            argument_gnuplot_file ,
            argument_range_of_files,
            argument_multipage_data_file,
            argument_data_process,
            argument_use_columns,
            argument_data_from_stdin,
            argument_repeat_frames_of_output,
            argument_from_image_to_data_file,
            argument_coords
           ]

{-- ================================================================================================
================================================================================================ --}
tag_DMap:: [String] -> DMap.Map String String
tag_DMap [] = DMap.fromList [
        --("",""),
        (argument_data_file,               default_data_file ),
        (argument_output_video_file,       default_output_video_file ),
        (argument_gnuplot_file,            default_gnuplot_file ),
        (argument_range_of_files,          default_range_of_files),
        (argument_test ,                   default_test),
        (argument_shakingAbacusTest,       default_shakingAbacusTest),
        (argument_data_bypass_mode,        default_data_bypass_mode),
        (argument_single_data_file,        default_single_data_file),
        (argument_multipage_data_file,     default_multipage_data_file),
        (argument_data_process ,           default_data_process),
        (argument_use_columns,             default_use_columns),
        (argument_data_from_stdin,         default_data_from_stdin),
        (argument_repeat_frames_of_output, default_repeat_frames_of_output),
        (argument_matrix_stacking,         default_matrix_stacking),
        (argument_from_image_to_data_file, default_from_image_to_data_file),
        (argument_coords,                  default_coords)
   ]----]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]

tag_DMap lst = DMap.union (DMap.fromList $ map (\(Just x) -> x) $ list_arguments lst) $
                                                                                       tag_DMap []
----------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
list_arguments :: [String] -> [Maybe (String, String)]
list_arguments [] = []
list_arguments (tag:rest)
  | take 2 tag == "--" && elem tag' flags =
                       (Just (tag', "true")) : list_arguments rest
  | take 2 tag == "--" && elem tag' options =
                       (Just (tag', after_tag)) : list_arguments rest'

  |otherwise = list_arguments rest

  where
     after_tag = head rest
     tag' = (drop 2 tag)

     rest'
        |rest /= [] = tail rest
        |otherwise = []
     rest''
        |rest' /= [] = tail rest'
        |otherwise = []
----------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
get_demanded_processors :: String -> [String]
get_demanded_processors arg = words $ map commas2spaces arg
   where
    commas2spaces :: Char -> Char
    commas2spaces c
       |c == ',' = ' '
       |otherwise = c
--------------------------------------------------------------------------------------------------

{-- ================================================================================================
================================================================================================ --}
get_demanded_columns :: String -> (Int, Int)
get_demanded_columns arg = (\x -> (read $ head x, read $ last x) ) $ break_to_columns arg $
                                                                               at_semicolons arg 0
    where
        at_semicolons :: String -> Int -> [Int]
        at_semicolons [] _ = []
        at_semicolons (x:rest) i
           |x == ':' = i:at_semicolons rest (i+1)
           |otherwise = at_semicolons rest (i+1)

        break_to_columns :: String -> [Int] -> [String]
        break_to_columns str [] = [str]
        break_to_columns str (i:rest) = (\(s,sr) -> s : (break_to_columns (tail sr) rest) )
                                                                                    $ splitAt i str

--------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
get_demanded_coords :: String -> ((Int, Int),(Int, Int))
get_demanded_coords arg = (\x -> (head x, last x) ) $ map read_cords $ break_to_columns arg $
                                                                             at_semicolons arg 0
   where
        at_semicolons :: String -> Int -> [Int]
        at_semicolons [] _ = []
        at_semicolons (x:rest) i
           |x == ':' = i:at_semicolons rest (i+1)
           |otherwise = at_semicolons rest (i+1)

        break_to_columns :: String -> [Int] -> [String]
        break_to_columns str [] = [str]
        break_to_columns str (i:rest) = (\(s,sr) -> s : (break_to_columns (tail sr) rest) )
                                                                                    $ splitAt i str

        commas2spaces :: Char -> Char
        commas2spaces c
           |c == ',' = ' '
           |otherwise = c

        read_cords :: String -> (Int, Int)
        read_cords c = (\x -> (read $ head x, read $ last x) ) $ words $ map commas2spaces c

----------------------------------------------------------------------------------------------------


