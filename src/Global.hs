
module Global (

argument_data_file ,
argument_single_data_file,
argument_multipage_data_file ,
argument_gnuplot_file ,
argument_range_of_files ,
argument_test,
argument_data_bypass_mode ,
argument_data_process,

default_data_file,
default_single_data_file,
default_multipage_data_file,
default_gnuplot_file,
default_range_of_files,
default_test,
default_data_bypass_mode,
default_data_process,

flags ,

options,

tag_DMap,

list_arguments,

eol_char

) where

import qualified Data.Map as DMap

argument_data_file = "data-file"
argument_single_data_file = "single-data-file"
argument_multipage_data_file = "multipage-data-file"
argument_gnuplot_file = "gnuplot-file"
argument_range_of_files = "range-of-files" -- if range [5..10] then read data5, data6, ... data10
argument_test = "test"
argument_data_bypass_mode = "data-bypass-mode"
argument_data_process = "data-process"

default_data_file = "data"
default_single_data_file = "false"
default_multipage_data_file = "-"
default_gnuplot_file = "plot.gpi"
default_range_of_files = "1..1000" -- if range [5..10] then read data5, data6, ... data10
default_test = "false"
default_data_bypass_mode = "false"
default_data_process = "-"

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
            argument_data_process
           ]

{-- ================================================================================================
================================================================================================ --}
tag_DMap::[String] -> DMap.Map String String
tag_DMap [] = DMap.fromList [
        --("",""),
        (argument_data_file,           default_data_file ),
        (argument_gnuplot_file,        default_gnuplot_file ),
        (argument_range_of_files,      default_range_of_files),
        (argument_test ,               default_test),
        (argument_data_bypass_mode,    default_data_bypass_mode),
        (argument_single_data_file,    default_single_data_file),
        (argument_multipage_data_file, default_multipage_data_file),
        (argument_data_process ,       default_data_process)
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
