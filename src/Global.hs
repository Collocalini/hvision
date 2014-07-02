
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
argument_matrix_stacking,

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
default_matrix_stacking,

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

flags ,

options,

tag_DMap,

list_arguments,

eol_char,

read_file_if_exists,

loadImage,

rgb2grayscale,
rgb2grayscale_io_maybe,
to_grayscale_io_maybe

) where

import qualified Data.Map as DMap
import qualified Codec.Picture as CPic
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
argument_matrix_stacking = "matrix-stacking"


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
default_matrix_stacking = "false"




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


flags = [
         argument_test,
         argument_data_bypass_mode,
         argument_single_data_file,
         argument_matrix_stacking
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
tag_DMap:: [String] -> DMap.Map String String
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
        (argument_repeat_frames_of_output, default_repeat_frames_of_output),
        (argument_matrix_stacking,         default_matrix_stacking)
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
       --handle <- openFile name ReadMode
       --c <- hGetContents handle
      -- hClose handle
       --c <- readFile name
       readFile name
       --return c

-------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
loadImage :: FilePath -> IO (Maybe (CPic.DynamicImage))--(Maybe (CPic.Image CPic.PixelRGB8))
loadImage name = do image <- CPic.readImage name
                    case image of
                      (Left s) -> do
                                    print s
                                    return Nothing
                                     --exitWith (ExitFailure 1)
                      (Right d) ->
                                 do
                                    return $ Just d -- $ fmt d
                                 --return  $ Just $ CPic.pixelAt ((\(CPic.ImageRGB8 i) -> i) d) 0 0
                                 --return $ Just d
  where
  fmt :: CPic.DynamicImage -> Maybe (CPic.Image CPic.PixelRGB8)
 -- fmt i
 --   |(CPic.ImageRGB8 i) = Just i
 --   |otherwise = Nothing
  fmt (CPic.ImageRGB8 i) = Just i
  fmt (_) = Nothing

       --(Maybe CPic.PixelRGB8) --(Maybe CPic.DynamicImage)
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
rgb2grayscale :: CPic.DynamicImage -> CPic.Image CPic.Pixel8
rgb2grayscale (CPic.ImageRGB8 img) = CPic.pixelMap step1 img
  where
  step1 :: CPic.PixelRGB8 -> CPic.Pixel8
  step1 (CPic.PixelRGB8 r g b) = div (r+g+b) 3


rgb2grayscale_maybe :: Maybe (CPic.DynamicImage) ->
                          Maybe (CPic.Image CPic.Pixel8)
rgb2grayscale_maybe Nothing = Nothing
rgb2grayscale_maybe (Just img) = Just $ rgb2grayscale img

rgb2grayscale_io_maybe :: IO (Maybe (CPic.DynamicImage)) ->
                          IO (Maybe ( CPic.Image CPic.Pixel8))
rgb2grayscale_io_maybe img = do i <- img
                                return $ rgb2grayscale_maybe i
----------------------------------------------------------------------------------------------------






{-- ================================================================================================
================================================================================================ --}
to_grayscale :: CPic.DynamicImage -> CPic.Image CPic.Pixel8
to_grayscale (CPic.ImageRGB8 img) = CPic.pixelMap step1 img
  where
  step1 :: CPic.PixelRGB8 -> CPic.Pixel8
  step1 (CPic.PixelRGB8 r g b) = div (r+g+b) 3


to_grayscale (CPic.ImageRGB16 img) = CPic.pixelMap step1 img
  where
  step1 :: CPic.PixelRGB16 -> CPic.Pixel8
  step1 (CPic.PixelRGB16 r16 g16 b16) = round $ ((rf+gf+bf) / 3) * ((2^8)/(2^16))
     where
     rf= fromIntegral r16
     gf= fromIntegral g16
     bf= fromIntegral b16

to_grayscale (CPic.ImageY8 img) = img


to_grayscale_maybe :: Maybe (CPic.DynamicImage) ->
                          Maybe (CPic.Image CPic.Pixel8)
to_grayscale_maybe Nothing = Nothing
to_grayscale_maybe (Just img) = Just $ to_grayscale img

to_grayscale_io_maybe :: IO (Maybe (CPic.DynamicImage)) ->
                          IO (Maybe ( CPic.Image CPic.Pixel8))
to_grayscale_io_maybe img = do i <- img
                               return $ to_grayscale_maybe i
----------------------------------------------------------------------------------------------------


























