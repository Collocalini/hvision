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

module Video (
data_process_ffmpeg,
VideoProcessing(..)
) where

import Control.DeepSeq
import Codec.FFmpeg
import Codec.FFmpeg.Juicy
import Data.Matrix
import qualified Data_iterators as DI
import Data.Dynamic
import qualified Codec.Picture as CPic
import Processors_common
import Control.Monad.State
import Image_loading
import Data.Conduit
import qualified Data.Conduit.List as CL

{--
data_file_v :: Maybe FilePath -> IO [ImageY8]
data_file_v Nothing = return []
data_file_v (Just tag_DMap) = sequence [read_file_if_exists (DMap.findWithDefault "Not found"
                                                                               ( argument_data_file)
                                                                                         tag_DMap)]
   step1 :: FilePath -> [IO ImageY8]
   step1 vidFile =
     do initFFmpeg
        (getFrame, cleanup) <- imageReaderTime vidFile
        --frame <- getFrame
        --cleanup

   --  where

   step2 :: IO (Maybe (Image p, Double) -> [IO ImageY8]
   step2 gf = do
     frame <- gf
     case frame of
       Just (avf,ts) -> return (ImageY8 avf):(step2 gf)
       Nothing -> return []
--}


{-- common steps in data_processM,
                    data_processMultipage,
                    data_processMultipage_fromStdin  functions
===============================================================================================
data_process_ffmpeg :: FilePath ->
                       DI.IterateData ->
                       [(Matrix Rational) -> (Matrix Rational)] ->
                       (CPic.Image CPic.Pixel8 -> (Matrix Rational) ) ->
                       ( (Matrix Rational) -> String) ->
                       IO ()
data_process_ffmpeg file itd processors adaptTo adaptFrom = do
     initFFmpeg
     (getFrame, cleanup) <- imageReaderTime file
     s2 <- step2 getFrame
     (_,_) <- runStateT (DI.iterate_all_data_v $ map step4 s2) itd
     cleanup
     where


     step2 :: IO (Maybe (CPic.Image CPic.Pixel8, Double)) -> IO [(CPic.Image CPic.Pixel8, Double)]
     step2 gf = do
        frame <- gf
        --replicateM 1 gf
        case frame of
           Just f ->  step2 gf >>= \s -> return $ f:s
           Nothing -> return []

     step4 :: (CPic.Image CPic.Pixel8, Double) -> String
     step4 (avf,_) = adaptFrom $ (apply_processors_v processors) $ adaptTo avf





----------------------------------------------------------------------------------------------------
--}


--type GetFrame = JuicyPixelFormat p => IO (Maybe (CPic.Image p, Double))
data VideoProcessing = VideoProcessing {
   data_file :: FilePath
  ,processors :: [(Matrix Rational) -> (Matrix Rational)]
  ,adaptFrom :: ( (Matrix Rational) -> String)
  ,itd :: DI.IterateData
  ,cleanup :: IO ()
--  ,getFrame :: GetFrame
   }


{--
===============================================================================================  --}
data_process_ffmpeg :: VideoProcessing -> IO ()
data_process_ffmpeg vp = do

   evalStateT (sssS $$ cccC) vp
   --putStrLn "5"
   where


sssS :: Source (StateT VideoProcessing IO) (CPic.Image CPic.Pixel8, Double)
sssS = do
   vp_state@(VideoProcessing {data_file = data_file}) <- get
   liftIO initFFmpeg
   (getFrame, cleanup) <- liftIO $ imageReaderTime data_file
   put ((\vp -> vp {cleanup = cleanup}) vp_state)
   --s2 <- liftIO $ step2 getFrame
   --CL.sourceList s2
   step2 getFrame
   where
   step2 :: IO (Maybe (CPic.Image CPic.Pixel8, Double)) -> Source (StateT VideoProcessing IO) (CPic.Image CPic.Pixel8, Double)
   step2 gf = do
        frame <- liftIO gf
        case frame of
           Just f ->  do yield f
                         step2 gf  -- >>= \s -> return $ f:s
           Nothing -> return ()


cccC :: Sink (CPic.Image CPic.Pixel8, Double) (StateT VideoProcessing IO) ()
cccC = do
   frame <- await
   (VideoProcessing {adaptFrom = adaptFrom
                    ,processors = processors
                    ,itd = itd
                    ,cleanup = cleanup}) <- get

   let step4 (avf,_) = adaptFrom $ (apply_processors_v processors) $ adaptTo avf

   case frame of
     Just frame -> do liftIO $! evalStateT (DI.iterate_all_data_v $ step4 frame) itd
                      cccC
     Nothing -> liftIO cleanup

   where
   adaptTo = imageToMatrix_rational

----------------------------------------------------------------------------------------------------






{--
-- | Decoding example. Try changing 'ImageRGB8' to 'ImageY8' in the
-- 'savePngImage' lines to automatically decode to grayscale images!
testDecode :: FilePath -> IO ()
testDecode vidFile =
  do initFFmpeg
     (getFrame, cleanup) <- imageReaderTime vidFile
     frame1 <- getFrame
     case frame1 of
       Just (avf,ts) -> do putStrLn $ "Frame at "++show ts
                           CPic.savePngImage "frame1.png" (CPic.ImageRGB8 avf)
       Nothing -> putStrLn "No frame for me :("
     replicateM_ 299 getFrame
     frame2 <- getFrame
     case frame2 of
       Just (avf,ts) -> do putStrLn $ "Frame at "++show ts
                           CPic.savePngImage "frame2.png" (CPic.ImageRGB8 avf)
       Nothing -> putStrLn "No frame for me :("
     cleanup
     putStrLn "All done!"--}

