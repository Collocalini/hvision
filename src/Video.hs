-----------------------------------------------------------------------------
--
-- Module      :  Video
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
--{-# LANGUAGE ImpredicativeTypes #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}

module Video (
data_process_ffmpeg,
VideoProcessing(..)
) where

--import Control.DeepSeq
import Codec.FFmpeg
--import Codec.FFmpeg.Juicy
--import Data.Matrix
import qualified Data_iterators as DI
--import Data.Dynamic
import qualified Codec.Picture as CPic
import Processors_common
import Processors2d
import Control.Monad.State
import Image_loading
import Data.Conduit
import qualified Data.ByteString.Lazy as B
--import qualified Data.Conduit.List as CL


-- State a ((Matrix Word8) -> (Matrix Word8))
--

data VideoProcessing = VideoProcessing {
   data_file :: FilePath
  ,output_video_file :: Maybe FilePath
  ,processors :: Processors
  ,processors' :: Processors'
  ,itd :: DI.IterateData
  ,cleanup :: IO ()
   }



{-- ================================================================================================
===============================================================================================  --}
data_process_ffmpeg :: VideoProcessing -> IO ()
data_process_ffmpeg vp@(VideoProcessing {output_video_file=Nothing}) = do evalStateT (readVideo $$ processVideoToString) vp
--data_process_ffmpeg vp@(VideoProcessing {output_video_file=Just "-"}) = do evalStateT (readVideo $$ processVideoToPngStdOut) vp
--data_process_ffmpeg vp@(VideoProcessing {output_video_file=Just "-"}) = do evalStateT (readVideo $$ processVsVideoToPngStdOut) vp
data_process_ffmpeg vp@(VideoProcessing {output_video_file=Just "-"}) = do evalStateT (readVideo $$ processVs_straightApply_VideoToPngStdOut) vp

data_process_ffmpeg vp = do evalStateT (readVideo $$ processVideoToVideo) vp
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
===============================================================================================  --}
readVideo :: Source (StateT VideoProcessing IO) (CPic.Image CPic.PixelRGB8, Double)
readVideo = do
   vp_state@(VideoProcessing {data_file = data_file}) <- get
   liftIO initFFmpeg
   (getFrame, cleanup) <- liftIO $ imageReaderTime data_file
   put ((\vp -> vp {cleanup = cleanup}) vp_state)
   step2 getFrame
   where
   step2 :: IO (Maybe (CPic.Image CPic.PixelRGB8, Double)) ->
                                 Source (StateT VideoProcessing IO) (CPic.Image CPic.PixelRGB8, Double)
   step2 gf = do
        frame <- liftIO gf
        case frame of
           Just f ->  do yield f
                         step2 gf  -- >>= \s -> return $ f:s
           Nothing -> return ()
----------------------------------------------------------------------------------------------------





{-- ================================================================================================
===============================================================================================  --}
processVideoToString :: Sink (CPic.Image CPic.PixelRGB8, Double) (StateT VideoProcessing IO) ()
processVideoToString = do
   frame <- await
   (VideoProcessing {processors = processors
                    ,itd = itd
                    ,cleanup = cleanup}) <- get

   case frame of
     Just frame -> do
                  liftIO $! evalStateT (DI.iterate_all_data_v $ processingPipe processors frame) itd
                  processVideoToString
     Nothing -> liftIO cleanup
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
===============================================================================================  --}
processVideoToVideo :: Sink (CPic.Image CPic.PixelRGB8, Double) (StateT VideoProcessing IO) ()
processVideoToVideo = do
   frame <- await
   (VideoProcessing {output_video_file = Just output_video_file
                    ,processors = processors
                    ,itd = itd
                    ,cleanup = cleanup}) <- get

   case frame of
     Just frame@((CPic.Image {CPic.imageWidth  = width
                            ,CPic.imageHeight = height}),_) -> do
        s <- liftIO $! imageWriter (encP (fromIntegral width) (fromIntegral height)) output_video_file
        let s' = (s :: Maybe (CPic.Image CPic.PixelRGB8) -> IO ()) . Just $ processingPipeVO processors frame
        liftIO s'
        processVideoToVideo_s2  (Just (width,height)) s
     Nothing -> do
        --s <- imageWriter (encP (fromIntegral w) (fromIntegral h)) output_video_file
        --liftIO $! imageWriterEnd' 0 0 s
        liftIO cleanup


processVideoToVideo_s2 :: Maybe (Int, Int) -> (Maybe (CPic.Image CPic.PixelRGB8) -> IO ()) ->
                             Sink (CPic.Image CPic.PixelRGB8, Double) (StateT VideoProcessing IO) ()
processVideoToVideo_s2 r@(Just (w,h)) s = do
   frame <- await
   (VideoProcessing {output_video_file = Just output_video_file
                    ,processors = processors
                    ,itd = itd
                    ,cleanup = cleanup}) <- get

   case frame of
     Just frame@(_,ts) -> do
        liftIO $! imageWriter' w h s frame processors
        liftIO $! putStrLn $ show ts
        processVideoToVideo_s2 r s
     Nothing -> do
        --liftIO $! imageWriterEnd' w h output_video_file
        liftIO cleanup


encP w h = EncodingParams {
                   epWidth  = w
                 , epHeight = h
                 , epFps    = 30
                 , epCodec  = Nothing --Just avCodecIdH264
                 , epPixelFormat = Nothing --Just avPixFmtYuv420p --Nothing
                 , epPreset = "medium"
                 }
----------------------------------------------------------------------------------------------------


imageWriterStart' :: Int -> Int -> FilePath -> (CPic.Image CPic.PixelRGB8, Double) -> Processors -> IO ()
imageWriterStart' w h f fr p = do
   s <- imageWriter (encP (fromIntegral w) (fromIntegral h)) f
   let s' = (s :: Maybe (CPic.Image CPic.PixelRGB8) -> IO ()) . Just $ processingPipeVO p fr
   s'

imageWriter' :: Int -> Int -> (Maybe (CPic.Image CPic.PixelRGB8) -> IO ()) -> (CPic.Image CPic.PixelRGB8, Double) -> Processors -> IO ()
imageWriter' w h s fr p = do
   let s' = s . Just $ processingPipeVO p fr
   s'

imageWriterEnd' :: Int -> Int -> (Maybe (CPic.Image CPic.PixelRGB8) -> IO ()) -> IO ()
imageWriterEnd' w h s = do
   let s' = s $ Nothing
   s'



--processingPipeVOVs (PMRational_l pmr) (avf,_) = toImageRGB8 $ (apply_processors_v_r pmr) $ to_grayscale_MF avf
--processingPipeVOVs (PMInt_l pmi)      (avf,_) = toImageRGB8 $ (apply_processors_v_i pmi) $ to_grayscale_MI avf


processingPipeVOVs pmb (avf,_) = (\(a, ps) -> (toImageRGB8 a,ps)) $ (apply_processors_vs_b pmb) $ to_grayscale_MI avf



processingPipeVO (PMRational_l pmr) (avf,_) = toImageRGB8 $ (apply_processors_v_r pmr) $ to_grayscale_MF avf
processingPipeVO (PMInt_l pmi)      (avf,_) = toImageRGB8 $ (apply_processors_v_i pmi) $ to_grayscale_MI avf
processingPipeVO (PMWord8_l pmb)    (avf,_) = toImageRGB8 $ (apply_processors_v_b pmb) $ to_grayscale_MI avf



processingPipe (PMRational_l pmr) (avf,_) = toString $ (apply_processors_v_r pmr) $ to_grayscale_MF avf
processingPipe (PMInt_l pmi)      (avf,_) = toString $ (apply_processors_v_i pmi) $ to_grayscale_MI avf
processingPipe (PMWord8_l pmb)    (avf,_) = toString $ (apply_processors_v_b pmb) $ to_grayscale_MI avf



{-- ================================================================================================
===============================================================================================  --}
processVideoToPngStdOut :: Sink (CPic.Image CPic.PixelRGB8, Double) (StateT VideoProcessing IO) ()
processVideoToPngStdOut = do
   frame <- await
   (VideoProcessing { --output_video_file = Just output_video_file
                     processors = processors
                    ,itd = itd
                    ,cleanup = cleanup}) <- get

   case frame of
     Just frame@(f@(CPic.Image {CPic.imageWidth  = width
                            ,CPic.imageHeight = height}),_) -> do
        liftIO $! B.putStr $ CPic.encodePng $ processingPipeVO processors frame
        processVideoToPngStdOut
     Nothing -> do
        liftIO cleanup




{-- ================================================================================================
===============================================================================================  --}
processVsVideoToPngStdOut :: Sink (CPic.Image CPic.PixelRGB8, Double) (StateT VideoProcessing IO) ()
processVsVideoToPngStdOut = do
   frame <- await
   vp_state@(VideoProcessing { --output_video_file = Just output_video_file
                     processors' = processors'
                    ,itd = itd
                    ,cleanup = cleanup}) <- get

   case frame of
     Just frame@(f,_) -> do

        (\(a,ps) -> do
           --liftIO $! putStr "*"
           liftIO $! B.putStr $ CPic.encodePng a
           put ((\vp -> vp {processors' = ps}) vp_state) ) $ processingPipeVOVs processors' frame

        processVsVideoToPngStdOut
     Nothing -> do
        liftIO cleanup


{-- ================================================================================================
===============================================================================================  --}
processVs_straightApply_VideoToPngStdOut :: Sink (CPic.Image CPic.PixelRGB8, Double) (StateT VideoProcessing IO) ()
processVs_straightApply_VideoToPngStdOut = do
   frame <- await
   vp_state@(VideoProcessing { --output_video_file = Just output_video_file
                     processors' = processors'
                    ,itd = itd
                    ,cleanup = cleanup}) <- get

   case frame of
     Just (f,_) -> do

        (\(a,ps) -> do
           --liftIO $! putStr "*"
           liftIO $! B.putStr $ CPic.encodePng a
           put ((\vp -> vp {processors' = ps}) vp_state) ) $ apply_processors_vs_rgb8  processors' f

        processVs_straightApply_VideoToPngStdOut
     Nothing -> do
        liftIO cleanup




