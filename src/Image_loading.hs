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

imageY8ToMatrix_fractional,

Image'(..),
--Image_loading.Image,
) where

import qualified Codec.Picture as CPic
import qualified Data.Matrix as DMatrix
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


{-- ================================================================================================
================================================================================================ --}
imageY8ToMatrix_fractional :: Fractional a => CPic.Image CPic.Pixel8 -> (DMatrix.Matrix a)
imageY8ToMatrix_fractional image@(CPic.Image {CPic.imageWidth  = width
                                           ,CPic.imageHeight = height}) =

   DMatrix.matrix width height $ \(x,y) -> fromIntegral $ CPic.pixelAt image (x-1) (y-1)
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
imageRGB8ToMatrix_fractional :: Fractional a => CPic.Image CPic.PixelRGB8 -> (DMatrix.Matrix a)
imageRGB8ToMatrix_fractional image@(CPic.Image {CPic.imageWidth  = width
                                             ,CPic.imageHeight = height}) =

   DMatrix.matrix width height $ \(x,y) ->  step1 $ CPic.pixelAt image (x-1) (y-1)

   where
   step1 :: Fractional a => CPic.PixelRGB8 -> a
   step1 (CPic.PixelRGB8 r8 g8 b8) = (rf+gf+bf) / 3
     where
     rf= fromIntegral r8
     gf= fromIntegral g8
     bf= fromIntegral b8
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
imageRGBA8ToMatrix_fractional :: Fractional a => CPic.Image CPic.PixelRGBA8 -> (DMatrix.Matrix a)
imageRGBA8ToMatrix_fractional image@(CPic.Image {CPic.imageWidth  = width
                                             ,CPic.imageHeight = height}) =

   DMatrix.matrix width height $ \(x,y) ->  step1 $ CPic.pixelAt image (x-1) (y-1)

   where
   step1 :: Fractional a => CPic.PixelRGBA8 -> a
   step1 (CPic.PixelRGBA8 r8 g8 b8 _) = (rf+gf+bf) / 3
     where
     rf= fromIntegral r8
     gf= fromIntegral g8
     bf= fromIntegral b8
----------------------------------------------------------------------------------------------------






{-- ================================================================================================
================================================================================================ --}
imageRGB16ToMatrix_fractional :: Fractional a => CPic.Image CPic.PixelRGB16 -> (DMatrix.Matrix a)
imageRGB16ToMatrix_fractional image@(CPic.Image {CPic.imageWidth  = width
                                              ,CPic.imageHeight = height}) =

   DMatrix.matrix width height $ \(x,y) ->  step1 $ CPic.pixelAt image (x-1) (y-1)

   where
   step1 :: Fractional a => CPic.PixelRGB16 -> a
   step1 (CPic.PixelRGB16 r16 g16 b16) = (rf+gf+bf) / 3
     where
     rf= fromIntegral r16
     gf= fromIntegral g16
     bf= fromIntegral b16
----------------------------------------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
imageRGBA16ToMatrix_fractional :: Fractional a => CPic.Image CPic.PixelRGBA16 -> (DMatrix.Matrix a)
imageRGBA16ToMatrix_fractional image@(CPic.Image {CPic.imageWidth  = width
                                              ,CPic.imageHeight = height}) =

   DMatrix.matrix width height $ \(x,y) ->  step1 $ CPic.pixelAt image (x-1) (y-1)

   where
   step1 :: Fractional a => CPic.PixelRGBA16 -> a
   step1 (CPic.PixelRGBA16 r16 g16 b16 _) = (rf+gf+bf) / 3
     where
     rf= fromIntegral r16
     gf= fromIntegral g16
     bf= fromIntegral b16
----------------------------------------------------------------------------------------------------







{-- ================================================================================================
================================================================================================ --}
imageY8ToMatrix_integral :: Integral a => CPic.Image CPic.Pixel8 -> (DMatrix.Matrix a)
imageY8ToMatrix_integral image@(CPic.Image {CPic.imageWidth  = width
                                           ,CPic.imageHeight = height}) =

   DMatrix.matrix width height $ \(x,y) -> fromIntegral $ CPic.pixelAt image (x-1) (y-1)
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
imageRGB8ToMatrix_integral :: Integral a => CPic.Image CPic.PixelRGB8 -> (DMatrix.Matrix a)
imageRGB8ToMatrix_integral image@(CPic.Image {CPic.imageWidth  = width
                                             ,CPic.imageHeight = height}) =

   DMatrix.matrix width height $ \(x,y) ->  step1 $ CPic.pixelAt image (x-1) (y-1)

   where
   step1 :: Integral a => CPic.PixelRGB8 -> a
   step1 (CPic.PixelRGB8 r8 g8 b8) = round $ (rf+gf+bf) / 3
     where
     rf= fromIntegral r8
     gf= fromIntegral g8
     bf= fromIntegral b8
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
imageRGBA8ToMatrix_integral :: Integral a => CPic.Image CPic.PixelRGBA8 -> (DMatrix.Matrix a)
imageRGBA8ToMatrix_integral image@(CPic.Image {CPic.imageWidth  = width
                                             ,CPic.imageHeight = height}) =

   DMatrix.matrix width height $ \(x,y) ->  step1 $ CPic.pixelAt image (x-1) (y-1)

   where
   step1 :: Integral a => CPic.PixelRGBA8 -> a
   step1 (CPic.PixelRGBA8 r8 g8 b8 _) = round $ (rf+gf+bf) / 3
     where
     rf= fromIntegral r8
     gf= fromIntegral g8
     bf= fromIntegral b8
----------------------------------------------------------------------------------------------------






{-- ================================================================================================
================================================================================================ --}
imageRGB16ToMatrix_integral :: Integral a => CPic.Image CPic.PixelRGB16 -> (DMatrix.Matrix a)
imageRGB16ToMatrix_integral image@(CPic.Image {CPic.imageWidth  = width
                                              ,CPic.imageHeight = height}) =

   DMatrix.matrix width height $ \(x,y) ->  step1 $ CPic.pixelAt image (x-1) (y-1)

   where
   step1 :: Integral a => CPic.PixelRGB16 -> a
   step1 (CPic.PixelRGB16 r16 g16 b16) = round $ (rf+gf+bf) / 3
     where
     rf= fromIntegral r16
     gf= fromIntegral g16
     bf= fromIntegral b16
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
imageRGBA16ToMatrix_integral :: Integral a => CPic.Image CPic.PixelRGBA16 -> (DMatrix.Matrix a)
imageRGBA16ToMatrix_integral image@(CPic.Image {CPic.imageWidth  = width
                                               ,CPic.imageHeight = height}) =

   DMatrix.matrix width height $ \(x,y) ->  step1 $ CPic.pixelAt image (x-1) (y-1)

   where
   step1 :: Integral a => CPic.PixelRGBA16 -> a
   step1 (CPic.PixelRGBA16 r16 g16 b16 _) = round $ (rf+gf+bf) / 3
     where
     rf= fromIntegral r16
     gf= fromIntegral g16
     bf= fromIntegral b16
----------------------------------------------------------------------------------------------------












--data Image' a = Image' a



--type Image'' = CPic.Image CPic.Pixel8
class Image' p where
   to_grayscale8 :: p -> CPic.Image CPic.Pixel8
   to_grayscale_MF :: Fractional a => p -> DMatrix.Matrix a
   to_grayscale_MI :: Integral a => p -> DMatrix.Matrix a
   --to_grayscale_M :: p -> DMatrix.Matrix a

instance Image' (CPic.Image CPic.Pixel8) where
   to_grayscale8 img = img
   to_grayscale_MF img = imageY8ToMatrix_fractional img
   to_grayscale_MI img = imageY8ToMatrix_integral img
   --to_grayscale_M ()

instance Image' (CPic.Image CPic.PixelRGB8) where
   to_grayscale8 img = CPic.pixelMap step1 img where
      step1 :: CPic.PixelRGB8 -> CPic.Pixel8
      step1 (CPic.PixelRGB8 r g b) = div (r+g+b) 3
   to_grayscale_MF img = imageRGB8ToMatrix_fractional img
   to_grayscale_MI img = imageRGB8ToMatrix_integral img

instance Image' (CPic.Image CPic.PixelRGBA8) where
   to_grayscale8 img = CPic.pixelMap step1 img where
      step1 :: CPic.PixelRGBA8 -> CPic.Pixel8
      step1 (CPic.PixelRGBA8 r g b _) = div (r+g+b) 3
   to_grayscale_MF img = imageRGBA8ToMatrix_fractional img
   to_grayscale_MI img = imageRGBA8ToMatrix_integral img

instance Image' (CPic.Image CPic.PixelRGB16) where
   to_grayscale8 img = CPic.pixelMap step1 img where
      step1 :: CPic.PixelRGB16 -> CPic.Pixel8
      step1 (CPic.PixelRGB16 r g b) = div (r'+g'+b') 3
        where
         r'= fromIntegral r
         g'= fromIntegral g
         b'= fromIntegral b
   to_grayscale_MF img = imageRGB16ToMatrix_fractional img
   to_grayscale_MI img = imageRGB16ToMatrix_integral img

instance Image' (CPic.Image CPic.PixelRGBA16) where
   to_grayscale8 img = CPic.pixelMap step1 img where
      step1 :: CPic.PixelRGBA16 -> CPic.Pixel8
      step1 (CPic.PixelRGBA16 r g b _) = div (r'+g'+b') 3
        where
         r'= fromIntegral r
         g'= fromIntegral g
         b'= fromIntegral b
   to_grayscale_MF img = imageRGBA16ToMatrix_fractional img
   to_grayscale_MI img = imageRGBA16ToMatrix_integral img








