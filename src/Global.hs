
module Global (

argument_data_file ,
argument_single_data_file,
argument_multipage_data_file ,
argument_data_from_stdin,
argument_gnuplot_file ,
argument_range_of_files ,
argument_test,
argument_data_bypass_mode ,
argument_data_process,
argument_use_columns,
argument_repeat_frames_of_output,

default_data_file,
default_single_data_file,
default_multipage_data_file,
default_data_from_stdin,
default_gnuplot_file,
default_range_of_files,
default_test,
default_data_bypass_mode,
default_data_process,
default_use_columns,
default_repeat_frames_of_output,

identity_i_processor',
identity_f_processor',
derivative_f_processor',
derivative_i_processor',
distance_between_extremums_f_processor',
extremums_f_processor',

flags ,

options,

tag_DMap,

list_arguments,

eol_char,

read_file_if_exists
) where

import qualified Data.Map as DMap
import System.IO

argument_data_file = "data-file"
argument_single_data_file = "single-data-file"
argument_multipage_data_file = "multipage-data-file"
argument_data_from_stdin = "data-from-stdin"
argument_gnuplot_file = "gnuplot-file"
argument_range_of_files = "range-of-files" -- if range [5..10] then read data5, data6, ... data10
argument_test = "test"
argument_data_bypass_mode = "data-bypass-mode"
argument_data_process = "data-process"
argument_use_columns = "use-columns"
argument_repeat_frames_of_output = "repeat-frames-of-output"

default_data_file = "data"
default_single_data_file = "false"
default_multipage_data_file = "-"
default_gnuplot_file = "plot.gpi"
default_range_of_files = "" -- if range [5..10] then read data5, data6, ... data10
default_test = "false"
default_data_bypass_mode = "false"
default_data_process = "-"
default_use_columns = "1:2"
default_data_from_stdin = "-"
default_repeat_frames_of_output = "1"





identity_i_processor' :: String -> Bool
identity_i_processor' str
   |"identity_i" == str = True
   |otherwise = False


identity_f_processor' :: String -> Bool
identity_f_processor' str
   |"identity_f" == str = True
   |otherwise = False


derivative_f_processor' :: String -> Bool
derivative_f_processor' str
   |"derivative_f" == str = True
   |otherwise = False


derivative_i_processor' :: String -> Bool
derivative_i_processor' str
   |"derivative_i" == str = True
   |otherwise = False


distance_between_extremums_f_processor' :: String -> Bool
distance_between_extremums_f_processor' str
   |"distance_between_extremums_f" == str = True
   |otherwise = False

extremums_f_processor' :: String -> Bool
extremums_f_processor' str
   |"extremums_f" == str = True
   |otherwise = False


flags = [
         argument_test,
         argument_data_bypass_mode,
         argument_single_data_file
        ]

options =  [
            argument_data_file ,
            argument_gnuplot_file ,
            argument_range_of_files,
            argument_multipage_data_file,
            argument_data_process,
            argument_use_columns,
            argument_data_from_stdin,
            argument_repeat_frames_of_output
           ]

{-- ================================================================================================
================================================================================================ --}
tag_DMap::[String] -> DMap.Map String String
tag_DMap [] = DMap.fromList [
        --("",""),
        (argument_data_file,               default_data_file ),
        (argument_gnuplot_file,            default_gnuplot_file ),
        (argument_range_of_files,          default_range_of_files),
        (argument_test ,                   default_test),
        (argument_data_bypass_mode,        default_data_bypass_mode),
        (argument_single_data_file,        default_single_data_file),
        (argument_multipage_data_file,     default_multipage_data_file),
        (argument_data_process ,           default_data_process),
        (argument_use_columns,             default_use_columns),
        (argument_data_from_stdin,         default_data_from_stdin),
        (argument_repeat_frames_of_output, default_repeat_frames_of_output)
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


eol_char = "\n"



{-- ================================================================================================
================================================================================================ --}
read_file_if_exists :: FilePath -> IO String
read_file_if_exists [] = do return ""
read_file_if_exists name  = do
       handle <- openFile name ReadMode
       c <- hGetContents handle
       return c
-------------------------------------------------------------

