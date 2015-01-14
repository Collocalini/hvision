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

{-- common steps in data_processM,
                    data_processMultipage,
                    data_processMultipage_fromStdin  functions
===============================================================================================  --}
data_process_ffmpeg ::State InputArguments ->
                                [([(Dynamic, Dynamic)] -> [ (Processor_data, Processor_data) ])] ->
                                (String -> [(Dynamic, Dynamic)] ) ->
                                [String] -> -- input
                                IO ()
data_process_ffmpeg tag_DMap processors adaptTo x = iterate_all_data tag_DMap $
                    map (toStringTable . stack_output . (apply_processors (processors)) . adaptTo) x

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

