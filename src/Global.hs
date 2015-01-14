
module Global (

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



eol_char,

sortLt_by_y,
sortGt_by_y,
sortLt_by_x,
sortGt_by_x,

read_file_if_exists,

loadImage,

rgb2grayscale,
rgb2grayscale_io_maybe,
to_grayscale_io_maybe

) where

--import qualified Data.Map as DMap
import qualified Codec.Picture as CPic
--import Data.Dynamic
import System.IO
--import Processors_common







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


histogram_y_per_pixel_multiple_rows_dft_f_processor' :: String -> Bool
histogram_y_per_pixel_multiple_rows_dft_f_processor' str
   |"histogram_y_per_pixel_multiple_rows_dft_f" == str = True
   |otherwise = False


ad_hock_f_processor' :: String -> Bool
ad_hock_f_processor' str
   |"ad_hock_f" == str = True
   |otherwise = False









eol_char = "\n"

{-- ================================================================================================
================================================================================================ --}
sortLt_by_y (_, ly) (_, ry)
  | ly < ry = GT
  | ly > ry = LT
  | ly == ry = EQ
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
sortGt_by_y (_, ly) (_, ry)
  | ly < ry = LT
  | ly > ry = GT
  | ly == ry = EQ
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
sortLt_by_x (lx, _) (rx, _)
  | lx < rx = GT
  | lx > rx = LT
  | lx == rx = EQ
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


--data FileDataType = Text|Image|Video


{-- ================================================================================================
================================================================================================ --}
{--read_file_if_exists_ :: FilePath -> FileDataType -> IO String
read_file_if_exists_ [] = do return ""
read_file_if_exists_ name  = do
       --handle <- openFile name ReadMode
       --c <- hGetContents handle
      -- hClose handle
       --c <- readFile name
       readFile name
       --return c
--}
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


























