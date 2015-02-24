-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (

main

) where

import System.IO

import System.Environment
import Data.List
import Data.Dynamic
import qualified Data.Map as DMap
import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.Char
--import System.Process
import Control.Monad
import Pipes
import qualified Pipes.Prelude as P
import qualified Data.Matrix as Dmatrix

---from this project
import Processors
--import Processors2d
import Processors_common
import Global
import Image_loading
import qualified Cmd_arguments as CmdA
import ImageManipulation
import Recognize_demanded_processors
import Data_iterators
import qualified Video as Vd
---end of imports from this project









weed_data_from_input :: Int -> [String] -> [String]
weed_data_from_input n str = concat $ map ((step1 []).(drop n).lines) str
    where
    step1 :: String -> [String]-> [String]
    step1 iteration []  = [iteration]
    step1 iteration (str:[])
       |((filter (' ' /=) str) == "EOF") = [iteration]
--       |((filter (' ' /=) str) == "") = [iteration]
       |otherwise = [step2 iteration str]

    step1 iteration (str:rest)
       |((filter (' ' /=) str) == "EOF") = iteration : step1 [] (drop n rest)
--       |((filter (' ' /=) str) == "") = iteration : step1 [] rest
       |otherwise = step1 (step2 iteration str) rest

    step2 :: String -> String -> String
    step2 [] str1 = str1
    step2 str str1 = str++eol_char++str1






{-- pass data to gnuplot (no processing) =========================================================
================================================================================================ --}
data_bypass :: DMap.Map String String -> [Int] -> IO ()
data_bypass  tag_DMap []  =  (data_file' $ data_file tag_DMap) >>= \x -> iterate_all_data tag_DMap x
data_bypass tag_DMap range = (data_file' $ data_file_range tag_DMap range) >>= \x ->
                                                                       iterate_all_data tag_DMap x

--------------------------------------------------------





{-- common steps in data_processM,
                    data_processMultipage,
                    data_processMultipage_fromStdin  functions
===============================================================================================  --}
data_process_common :: DMap.Map String String ->
                                [([(Dynamic, Dynamic)] -> [ (Processor_data, Processor_data) ])] ->
                                (String -> [(Dynamic, Dynamic)] ) ->
                                [String] -> -- input
                                IO ()
data_process_common tag_DMap processors adaptTo x = iterate_all_data tag_DMap $
                    map (toStringTable . stack_output . (apply_processors (processors)) . adaptTo) x

----------------------------------------------------------------------------------------------------





{-- common steps in data_process_range_sensitive
===============================================================================================  --}
data_process_common_range_sensitive :: DMap.Map String String ->
                             [([[(Dynamic, Dynamic)]] -> [[ (Processor_data, Processor_data) ]])] ->
                             (String -> [(Dynamic, Dynamic)] ) ->
                             [String] -> -- input
                             IO ()
data_process_common_range_sensitive tag_DMap processors adaptTo x = iterate_all_data tag_DMap $
  map (toStringTable . stack_output) $ apply_processors_context_sensitive processors $ map adaptTo x

----------------------------------------------------------------------------------------------------

{-- common steps in data_process_range_sensitive_matrix_output
===============================================================================================  --}
data_process_common_range_sensitive_matrix_output :: DMap.Map String String ->
                             [([[(Dynamic, Dynamic)]] -> [[ (Processor_data, Processor_data) ]])] ->
                             (String -> [(Dynamic, Dynamic)] ) ->
                             [String] -> -- input
                             IO ()
data_process_common_range_sensitive_matrix_output tag_DMap processors adaptTo x =
  iterate_all_data tag_DMap [toStringTable_matrix $ stack_output_matrix $
                                      apply_processors_context_sensitive processors $ map adaptTo x]

----------------------------------------------------------------------------------------------------


{-- common steps in data_processMultipage_fromStdin_matrix_output
===============================================================================================  --}
data_process_common_matrix_output :: DMap.Map String String ->
                                [([(Dynamic, Dynamic)] -> [ (Processor_data, Processor_data) ])] ->
                                (String -> [(Dynamic, Dynamic)] ) ->
                                [String] -> -- input
                                IO ()
data_process_common_matrix_output tag_DMap processors adaptTo x = iterate_all_data tag_DMap $
      [toStringTable_matrix $ stack_output_matrix $ map ((apply_processors (processors)) . adaptTo) x]

----------------------------------------------------------------------------------------------------





{-- process and pass data to gnuplot +=============================================================
================================================================================================ --}

data_processM :: DMap.Map String String -> [Int] ->
                                [([(Dynamic, Dynamic)] -> [ (Processor_data, Processor_data) ])] ->
                                (String -> [(Dynamic, Dynamic)] ) ->
                                IO ()
data_processM  tag_DMap [] processors adaptTo =
    (data_file' $ data_file tag_DMap) >>= \x -> data_process_common tag_DMap processors adaptTo x

data_processM tag_DMap range processors adaptTo = (data_file' $
   data_file_range tag_DMap range) >>= \x -> data_process_common tag_DMap processors adaptTo x
--------------------------------------------------------





{-- process and pass data to gnuplot +=============================================================
================================================================================================ --}

data_processMultipage :: DMap.Map String String -> [Int] ->
                                [([(Dynamic, Dynamic)] -> [ (Processor_data, Processor_data) ])] ->
                                (String -> [(Dynamic, Dynamic)] ) ->
                                ([String] -> [String]) ->
                                IO ()
data_processMultipage  tag_DMap [] processors adaptTo prepare_input =
    (data_file' $ data_file tag_DMap) >>= \x -> data_process_common tag_DMap processors adaptTo $
                                                                                     prepare_input x

data_processMultipage tag_DMap range processors adaptTo prepare_input = (data_file' $
   data_file_range tag_DMap range) >>= \x -> data_process_common tag_DMap processors adaptTo $
                                                                                     prepare_input x
--------------------------------------------------------





{-- process and pass data to gnuplot +=============================================================
================================================================================================ --}

data_processMultipage_fromStdin :: DMap.Map String String -> [Int] ->
                                [([(Dynamic, Dynamic)] -> [ (Processor_data, Processor_data) ])] ->
                                (String -> [(Dynamic, Dynamic)] ) ->
                                ([String] -> [String]) ->
                                Consumer [String] IO ()
data_processMultipage_fromStdin  tag_DMap [] processors adaptTo prepare_input = forever $
    (await) >>= \x -> lift $ data_process_common tag_DMap processors adaptTo $ prepare_input x
----------------------------------------------------------------------------------------------------





{-- process data when context of data frame is important (Like when processing a time frame).
 ============= And then pass data to gnuplot =======WARNING n_o_t compatible with rest=== --}
data_process_range_sensitive :: DMap.Map String String -> [Int] ->
                           [([[(Dynamic, Dynamic)]] -> [[ (Processor_data, Processor_data) ]] )] ->
                               (String -> [(Dynamic, Dynamic)] ) ->
                               ([String] -> [String]) ->
                               IO ()
data_process_range_sensitive  tag_DMap [] processors adaptTo prepare_input =
   (data_file' $ data_file tag_DMap) >>= \x -> data_process_common_range_sensitive tag_DMap
                                                                                 processors
                                                                                 adaptTo $
                                                                                 prepare_input x

data_process_range_sensitive tag_DMap range processors adaptTo prepare_input =
   (data_file' $ data_file_range tag_DMap range) >>= \x -> data_process_common_range_sensitive
                                                                                 tag_DMap
                                                                                 processors
                                                                                 adaptTo $
                                                                                 prepare_input x
----------------------------------------------------------------------------------------------------





{-- process data when context of data frame is important (Like when processing a time frame).
 ============= And then pass data to gnuplot =======WARNING n_o_t compatible with rest=== --}
data_process_range_sensitive_matrix_output :: DMap.Map String String -> [Int] ->
                           [([[(Dynamic, Dynamic)]] -> [[ (Processor_data, Processor_data) ]] )] ->
                               (String -> [(Dynamic, Dynamic)] ) ->
                               ([String] -> [String]) ->
                               IO ()
data_process_range_sensitive_matrix_output  tag_DMap [] processors adaptTo prepare_input =
   (data_file' $ data_file tag_DMap) >>= \x -> data_process_common_range_sensitive_matrix_output
                                                                                 tag_DMap
                                                                                 processors
                                                                                 adaptTo $
                                                                                 prepare_input x

data_process_range_sensitive_matrix_output tag_DMap range processors adaptTo prepare_input =
   (data_file' $ data_file_range tag_DMap range) >>= \x ->
                                             data_process_common_range_sensitive_matrix_output
                                                                                 tag_DMap
                                                                                 processors
                                                                                 adaptTo $
                                                                                 prepare_input x
----------------------------------------------------------------------------------------------------











{-- process and pass data to gnuplot +=============================================================
================================================================================================ --}

data_processMultipage_fromStdin_matrix_output :: DMap.Map String String -> [Int] ->
                                [([(Dynamic, Dynamic)] -> [ (Processor_data, Processor_data) ])] ->
                                (String -> [(Dynamic, Dynamic)] ) ->
                                ([String] -> [String]) ->
                                Consumer [String] IO ()
data_processMultipage_fromStdin_matrix_output  tag_DMap [] processors adaptTo prepare_input =
    forever $ (await) >>=
    \x -> lift $ data_process_common_matrix_output tag_DMap processors adaptTo $ prepare_input x
--------------------------------------------------------


{-- process and pass data to gnuplot +=============================================================
================================================================================================ --}

data_processMultipage_matrix_output :: DMap.Map String String -> [Int] ->
                                [([(Dynamic, Dynamic)] -> [ (Processor_data, Processor_data) ])] ->
                                (String -> [(Dynamic, Dynamic)] ) ->
                                ([String] -> [String]) ->
                                IO ()
data_processMultipage_matrix_output  tag_DMap [] processors adaptTo prepare_input =
    (data_file' $ data_file tag_DMap) >>= \x -> data_process_common_matrix_output tag_DMap processors
                                                                           adaptTo $ prepare_input x

data_processMultipage_matrix_output tag_DMap range processors adaptTo prepare_input = (data_file' $
   data_file_range tag_DMap range) >>= \x -> data_process_common_matrix_output tag_DMap processors
                                                                           adaptTo $ prepare_input x
----------------------------------------------------------------------------------------------------














{-- ================================================================================================
================================================================================================ --}
routine:: [String] -> IO ()
routine args
  |is_for_test = justtest
  |is_for_bypass = data_bypass tag_DMap' range
  |from_image_to_data_file = do_from_image_to_data_file
  |there_is_processing = do_processing
  |otherwise = return ()
   {-- |
       |
       |
       |
       V  --}
     where
     justtest = do  putStrLn "test"



     is_for_test :: Bool
     is_for_test
        |"true" == (DMap.findWithDefault "Not found" CmdA.argument_test $ tag_DMap') = True
        |otherwise = False


     is_multipage :: Bool
     is_multipage
        |(not has_frame_context_sensitive) && CmdA.default_multipage_data_file /=
          (DMap.findWithDefault CmdA.default_multipage_data_file CmdA.argument_multipage_data_file $
                                                                                   tag_DMap') = True
        |otherwise = False

     is_from_stdin :: Bool
     is_from_stdin
        |CmdA.default_data_from_stdin /= (DMap.findWithDefault CmdA.default_data_from_stdin
                                                 CmdA.argument_data_from_stdin $ tag_DMap') = True
        |otherwise = False

     has_frame_context_sensitive :: Bool
     has_frame_context_sensitive = -- True
        hasit $ recognizeDemanded_processors_frame_context_sensitive $ CmdA.get_demanded_processors
                  (DMap.findWithDefault CmdA.default_data_process CmdA.argument_data_process tag_DMap')

        where
        hasit :: [a] -> Bool
        hasit [] = False
        --hasit [] = False
        hasit _  = True


     has_2d_video :: Bool
     has_2d_video = -- True
        (hasit $ (\(CmdA.InputArguments {CmdA.data_process_v = d}) -> d) inputArgs')
          || (hasit' $ (\(CmdA.InputArguments {CmdA.data_process_vs = d}) -> d) inputArgs')
        where
        hasit :: Maybe Processors -> Bool
        --hasit (Just []) = False
        hasit Nothing = False
        hasit _  = True

        hasit' :: Maybe Processors' -> Bool
        --hasit (Just []) = False
        hasit' Nothing = False
        hasit' _  = True

     matrix_stacking_required :: Bool
     matrix_stacking_required
        |CmdA.default_matrix_stacking /= (DMap.findWithDefault CmdA.default_matrix_stacking
                                                 CmdA.argument_matrix_stacking $ tag_DMap') = True
        |otherwise = False


     is_for_bypass :: Bool
     is_for_bypass
        |"true" == (DMap.findWithDefault "Not found" CmdA.argument_data_bypass_mode $ tag_DMap') = True
        |otherwise = False

     from_image_to_data_file :: Bool
     from_image_to_data_file
        |CmdA.default_from_image_to_data_file /= (DMap.findWithDefault CmdA.default_from_image_to_data_file
                                               CmdA.argument_from_image_to_data_file $ tag_DMap') = True
        |otherwise = False


     do_from_image_to_data_file = (to_grayscale_io_maybe $ loadImage img) >>= (\i ->
             (\(l,r) ->
                iterate_all_data tag_DMap' $ map (toStringTable . stack_output_each_xy)
                [
                  map p $ pixels_along_the_line_maybe' i l r
                ]

             ) $ c

             )

          where
          img = (DMap.findWithDefault CmdA.default_from_image_to_data_file
                                               CmdA.argument_from_image_to_data_file $ tag_DMap')
          c = CmdA.get_demanded_coords (DMap.findWithDefault CmdA.default_coords CmdA.argument_coords $ tag_DMap')

          p :: [(Float,Float)] -> [(Processor_data, Processor_data)]
          p [] = []
          p i = identity_f_dyn $ map (\(x, y) -> ((toDyn x), (toDyn y)) ) i
  {----}

-----peculier section , there is processing to do
     there_is_processing :: Bool
     there_is_processing
        |CmdA.default_data_process /= (DMap.findWithDefault CmdA.default_data_process CmdA.argument_data_process
                                                                                 tag_DMap') = True
        |otherwise = False



     do_processing -- = context_sencitiveDefault
        |is_multipage && (not matrix_stacking_required)  = multipageDefault
        |is_multipage && matrix_stacking_required      = multipageMatrixStacking

        |is_from_stdin && (not matrix_stacking_required) = stdInDefault
        |is_from_stdin && matrix_stacking_required     = stdInMatrixStacking

        |has_frame_context_sensitive && (not matrix_stacking_required) = context_sensitiveDefault
        |has_frame_context_sensitive && matrix_stacking_required    = context_sensitiveMatrixStacking

        |has_2d_video = _2d_video

        |otherwise = oldDefault
   {--      --}



     multipageDefault = data_processMultipage tag_DMap' range
                       (
                       recognizeDemanded_processors $
                       CmdA.get_demanded_processors
                       (DMap.findWithDefault CmdA.default_data_process CmdA.argument_data_process tag_DMap')
                       )
                       (stringToFloatList_mn_dyn column_m column_n)
                       (weed_data_from_input  $ read
                                                   (DMap.findWithDefault CmdA.default_multipage_data_file
                                                           CmdA.argument_multipage_data_file $ tag_DMap')
                       )

     oldDefault = data_processM tag_DMap' range
                       (
                       recognizeDemanded_processors $
                       CmdA.get_demanded_processors
                       (DMap.findWithDefault CmdA.default_data_process CmdA.argument_data_process tag_DMap')
                       )
                       (stringToFloatList_mn_dyn column_m column_n)


     multipageMatrixStacking =  data_processMultipage_matrix_output tag_DMap' range
                       (
                       recognizeDemanded_processors $
                       CmdA.get_demanded_processors
                       (DMap.findWithDefault CmdA.default_data_process CmdA.argument_data_process tag_DMap')
                       )
                       (stringToFloatList_mn_dyn column_m column_n)
                       (weed_data_from_input  $ read
                                                   (DMap.findWithDefault CmdA.default_multipage_data_file
                                                           CmdA.argument_multipage_data_file $ tag_DMap')
                       )


     stdInDefault = runEffect $ P.stdinLn >-> (wait_for_piece_of_data []) >->
                     data_processMultipage_fromStdin tag_DMap' range
                       (
                       recognizeDemanded_processors $
                       CmdA.get_demanded_processors
                       (DMap.findWithDefault CmdA.default_data_process CmdA.argument_data_process tag_DMap')
                       )
                       (stringToFloatList_mn_dyn column_m column_n)
                       (weed_data_from_input  $ read
                                                   (DMap.findWithDefault CmdA.default_data_from_stdin
                                                           CmdA.argument_data_from_stdin $ tag_DMap')
                       )

     stdInMatrixStacking =
                     runEffect $ P.stdinLn >-> (wait_for_piece_of_data []) >->
                     data_processMultipage_fromStdin_matrix_output tag_DMap' range
                       (
                       recognizeDemanded_processors $
                       CmdA.get_demanded_processors
                       (DMap.findWithDefault CmdA.default_data_process CmdA.argument_data_process tag_DMap')
                       )
                       (stringToFloatList_mn_dyn column_m column_n)
                       (weed_data_from_input  $ read
                                                   (DMap.findWithDefault CmdA.default_data_from_stdin
                                                           CmdA.argument_data_from_stdin $ tag_DMap')
                       )


     context_sensitiveDefault = data_process_range_sensitive tag_DMap' range
                       (
                       recognizeDemanded_processors_frame_context_sensitive $
                       CmdA.get_demanded_processors
                       (DMap.findWithDefault CmdA.default_data_process CmdA.argument_data_process tag_DMap')
                       )
                       (stringToFloatList_mn_dyn column_m column_n)
                       (weed_data_from_input  $ read
                                                   (DMap.findWithDefault CmdA.default_multipage_data_file
                                                           CmdA.argument_multipage_data_file $ tag_DMap')
                       )

     context_sensitiveMatrixStacking = data_process_range_sensitive_matrix_output tag_DMap' range
                       (
                       recognizeDemanded_processors_frame_context_sensitive $
                       CmdA.get_demanded_processors
                       (DMap.findWithDefault CmdA.default_data_process CmdA.argument_data_process tag_DMap')
                       )
                       (stringToFloatList_mn_dyn column_m column_n)
                       (weed_data_from_input  $ read
                                                   (DMap.findWithDefault CmdA.default_multipage_data_file
                                                           CmdA.argument_multipage_data_file $ tag_DMap')
                       )



     _2d_video = Vd.data_process_ffmpeg
       (Vd.VideoProcessing {
                              Vd.data_file = dfile
                             ,Vd.processors = proc
                             ,Vd.processors' = proc'
                             ,Vd.output_video_file = ovfile
                             ,Vd.itd = itd
                             ,Vd.cleanup = return ()}
                             )
        where
        dfile = (\(CmdA.InputArguments {CmdA.data_file = (Just d)}) -> d) inputArgs'
        ovfile = (\(CmdA.InputArguments {CmdA.output_video_file = ( d)}) -> d) inputArgs'
        proc  = (\(CmdA.InputArguments {CmdA.data_process_v = (Just d)}) -> d) inputArgs'
        proc'  = (\(CmdA.InputArguments {CmdA.data_process_vs = (Just d)}) -> d) inputArgs'
        gfile = (\(CmdA.InputArguments {CmdA.gnuplot_file = g}) -> g) inputArgs'
        rfo   = (\(CmdA.InputArguments {CmdA.repeat_frames_of_output = r}) -> r) inputArgs'
        itd   = IterateData {gnuplot_file = gfile, repeat_frames_of_output = rfo}


-----end of peculier section


     column_m = (\(x, y) -> x) $ CmdA.get_demanded_columns
                       (DMap.findWithDefault CmdA.default_use_columns CmdA.argument_use_columns tag_DMap')

     column_n = (\(x, y) -> y) $ CmdA.get_demanded_columns
                       (DMap.findWithDefault CmdA.default_use_columns CmdA.argument_use_columns tag_DMap')

     tag_DMap' = CmdA.tag_DMap args
     range = get_range (DMap.findWithDefault "" CmdA.argument_range_of_files $ tag_DMap')
     inputArgs' = CmdA.inputArgs $ CmdA.tag_DMap args

----------------------------------------------------------------------------------------------------



wait_for_piece_of_data :: String -> Pipe String ( [String]) IO ()
wait_for_piece_of_data prev = do
   str <- await
   case str of
      "EOF" -> do yield [prev++str++"\n"]
                  wait_for_piece_of_data []
      otherwise -> wait_for_piece_of_data (prev++str++"\n")



main = do

    getArgs >>= \args -> routine args
    --putStr ""
    --test12
