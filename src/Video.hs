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
--import qualified Data.Conduit.List as CL





data VideoProcessing = VideoProcessing {
   data_file :: FilePath
  ,processors :: Processors
  ,itd :: DI.IterateData
  ,cleanup :: IO ()
   }



{--
===============================================================================================  --}
data_process_ffmpeg :: VideoProcessing -> IO ()
data_process_ffmpeg vp = do

   evalStateT (readVideo $$ processVideo) vp
   where


readVideo :: Source (StateT VideoProcessing IO) (CPic.Image CPic.Pixel8, Double)
readVideo = do
   vp_state@(VideoProcessing {data_file = data_file}) <- get
   liftIO initFFmpeg
   (getFrame, cleanup) <- liftIO $ imageReaderTime data_file
   put ((\vp -> vp {cleanup = cleanup}) vp_state)
   step2 getFrame
   where
   step2 :: IO (Maybe (CPic.Image CPic.Pixel8, Double)) ->
                                 Source (StateT VideoProcessing IO) (CPic.Image CPic.Pixel8, Double)
   step2 gf = do
        frame <- liftIO gf
        case frame of
           Just f ->  do yield f
                         step2 gf  -- >>= \s -> return $ f:s
           Nothing -> return ()


processVideo :: Sink (CPic.Image CPic.Pixel8, Double) (StateT VideoProcessing IO) ()
processVideo = do
   frame <- await
   (VideoProcessing { --adaptFrom = adaptFrom
                    processors = processors
                    ,itd = itd
                    ,cleanup = cleanup}) <- get

   case frame of
     Just frame -> do liftIO $! evalStateT (DI.iterate_all_data_v $ step2 processors frame) itd
                      processVideo
     Nothing -> liftIO cleanup

  where
    step2 (PMRational pmr) (avf,_) = toString $ (apply_processors_v_r pmr) $ to_grayscale_MF avf
    step2 (PMInt pmi)      (avf,_) = toString $ (apply_processors_v_i pmi) $ to_grayscale_MI avf

----------------------------------------------------------------------------------------------------



