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

) where

import Codec.FFmpeg
import Data.Matrix

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


data VideoProcessing = VideoProcessing {
   data_file :: Maybe FilePath
  ,data_process :: Maybe [( [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)] )]
   }
--}
{-- common steps in data_processM,
                    data_processMultipage,
                    data_processMultipage_fromStdin  functions
===============================================================================================  --}
data_process_ffmpeg :: State VideoProcessing
                       [(Matrix Rational) -> (Matrix Rational)] ->
                       (ImageY8 -> (Matrix Rational) ) ->
                       ( (Matrix Rational) -> String) ->
                       IO ()
data_process_ffmpeg processors adaptTo adaptFrom = do
     initFFmpeg
     (getFrame, cleanup) <- imageReaderTime vidFile
     step1 getFrame
     cleanup
     where
     step1 :: IO (Maybe (Image p, Double) -> [IO ImageY8]
     step1 gf = do
        frame <- gf
        case frame of
           Just (avf,ts) -> return (ImageY8 avf):(step1 gf)
           Nothing -> return []

----------------------------------------------------------------------------------------------------



-- | Decoding example. Try changing 'ImageRGB8' to 'ImageY8' in the
-- 'savePngImage' lines to automatically decode to grayscale images!
testDecode :: FilePath -> IO ()
testDecode vidFile =
  do initFFmpeg
     (getFrame, cleanup) <- imageReaderTime vidFile
     frame1 <- getFrame
     case frame1 of
       Just (avf,ts) -> do putStrLn $ "Frame at "++show ts
                           savePngImage "frame1.png" (ImageRGB8 avf)
       Nothing -> putStrLn "No frame for me :("
     replicateM_ 299 getFrame
     frame2 <- getFrame
     case frame2 of
       Just (avf,ts) -> do putStrLn $ "Frame at "++show ts
                           savePngImage "frame2.png" (ImageRGB8 avf)
       Nothing -> putStrLn "No frame for me :("
     cleanup
     putStrLn "All done!"

