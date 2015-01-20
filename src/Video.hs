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
) where

import Codec.FFmpeg
--import Codec.FFmpeg.Juicy
import Data.Matrix
import qualified Data_iterators as DI
import Data.Dynamic
import qualified Codec.Picture as CPic
import Processors_common
import Control.Monad.State


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
--type GetFrame = JuicyPixelFormat p => IO (Maybe (CPic.Image p, Double))
data VideoProcessing = VideoProcessing {
   data_file :: Maybe FilePath
  ,data_process :: Maybe [( [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)] )]
--  ,getFrame :: GetFrame
   }

{-- common steps in data_processM,
                    data_processMultipage,
                    data_processMultipage_fromStdin  functions
===============================================================================================  --}
data_process_ffmpeg :: FilePath ->
                       DI.IterateData ->
                       [(Matrix Rational) -> (Matrix Rational)] ->
                       (CPic.Image p -> (Matrix Rational) ) ->
                       ( (Matrix Rational) -> String) ->
                       IO ()
data_process_ffmpeg file itd processors adaptTo adaptFrom = do
     initFFmpeg
     (getFrame, cleanup) <- imageReaderTime file
     (l,r) <- runStateT (DI.iterate_all_data_v $ step2 getFrame) itd
     cleanup
     where

     {--step1 :: IO [String]
     step1 = do
        (step2 getFrame):(step1)
     --}


     step2 :: IO (Maybe (CPic.Image p, Double)) -> IO String
     step2 gf = do
        frame <- gf
        --replicateM 1 gf
        case frame of
           Just (avf,ts) -> return $ adaptFrom $ (apply_processors_v processors) $ adaptTo avf
           Nothing -> return ""


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

