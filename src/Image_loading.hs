-----------------------------------------------------------------------------
--
-- Module      :  Image_loading
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
{-# LANGUAGE FlexibleInstances #-}

module Image_loading (
loadImage,

rgb2grayscale,
rgb2grayscale_io_maybe,
to_grayscale_io_maybe,
to_grayscale,

Image'(..),
--Image_loading.Image,
) where

import qualified Codec.Picture as CPic
--import Codec.Picture

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



--data Image' a = Image' a



--type Image'' = CPic.Image CPic.Pixel8
class Image' p where
   to_grayscale8 :: p -> CPic.Image CPic.Pixel8

instance Image' (CPic.Image CPic.Pixel8) where
   to_grayscale8 img = img

instance Image' (CPic.Image CPic.PixelRGB8) where
   to_grayscale8 img = CPic.pixelMap step1 img where
      step1 :: CPic.PixelRGB8 -> CPic.Pixel8
      step1 (CPic.PixelRGB8 r g b) = div (r+g+b) 3

instance Image' (CPic.Image CPic.PixelRGBA8) where
   to_grayscale8 img = CPic.pixelMap step1 img where
      step1 :: CPic.PixelRGBA8 -> CPic.Pixel8
      step1 (CPic.PixelRGBA8 r g b _) = div (r+g+b) 3



