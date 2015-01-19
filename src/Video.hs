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

data VideoProcessing = VideoProcessing {
   data_file :: Maybe FilePath
  ,data_process :: Maybe [( [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)] )]
  ,getFrame :: IO (Maybe (CPic.Image, Double))
   }

{-- common steps in data_processM,
                    data_processMultipage,
                    data_processMultipage_fromStdin  functions
===============================================================================================  --}
data_process_ffmpeg :: [(Matrix Rational) -> (Matrix Rational)] ->
                       (CPic.ImageY8 -> (Matrix Rational) ) ->
                       ( (Matrix Rational) -> String) ->
                       StateT VideoProcessing IO ()
data_process_ffmpeg processors adaptTo adaptFrom = do
         (VideoProcessing {data_file = vp_df
                          ,data_process = vp_dp}) <- get
         step3 vp_df vp_dp
     where

     step3 :: Maybe FilePath -> Maybe [( [(Dynamic, Dynamic)] ->
              [(Processor_data, Processor_data)] )] ->
              StateT VideoProcessing IO ()
     step3 (Just fp) (Just pr) = do
         initFFmpeg
         (getFrame, cleanup) <- imageReaderTime fp
         DI.iterate_all_data_v $ mapM adaptFrom $ mapM step1 $ step2 getFrame
         cleanup
         where
         step1 :: IO CPic.ImageY8 -> IO Matrix Rational
         step1 frame = do
            return $  (apply_processors_v $ adaptTo frame)


         step2 :: IO (Maybe (CPic.Image p, Double)) -> IO [CPic.ImageY8]
         step2 gf = do
            frame <- gf
            case frame of
               Just (avf,ts) -> return $ (CPic.ImageY8 avf):(step2 gf)
               Nothing -> return []


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

