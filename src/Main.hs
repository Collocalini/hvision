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

---from this project
import Processors
import Global
---end of imports from this project




gnuplot_file :: DMap.Map String String -> IO String
gnuplot_file tag_DMap = read_file_if_exists (DMap.findWithDefault "Not found"
                                                                 ( argument_gnuplot_file) tag_DMap)

data_file :: DMap.Map String String -> [IO String]
data_file tag_DMap = [read_file_if_exists (DMap.findWithDefault "Not found" ( argument_data_file)
                                                                                         tag_DMap)]
data_file_range :: DMap.Map String String -> [Int] -> [IO String]
data_file_range _ [] = []
data_file_range tag_DMap range = map read_file_if_exists $ map ((DMap.findWithDefault "Not found"
           ( argument_data_file) $ tag_DMap) ++) $ map show range

get_range :: String -> [Int]
get_range [] = []
get_range range = (\(x,y) -> [(read x)..(read $ drop 2 y)]) $  splitAt
                                               ((\(Just x) -> x) (findIndex (== '.') range)) range

data_file':: [IO String] -> IO [String]
data_file' [] = return []
data_file' data_file = step1 $ data_file
      where
        step1 :: [IO String] -> IO [String]
        step1 [] = return []
        step1 (x:rest) = liftM2 (:) x (step1 rest)

iterate_all_data' :: DMap.Map String String ->  [String] -> IO ()
iterate_all_data' _ [] = return ()
iterate_all_data' tag_DMap x  = iterate_all_data tag_DMap x





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




{-- ================================================================================================
================================================================================================ --}
iterate_all_data :: DMap.Map String String -> [String] ->  IO ()
iterate_all_data _ [] = return ()
iterate_all_data tag_DMap (x:rest)  = do
    data_iterator tag_DMap (x:rest) repeats
    where
        repeats = read $ (DMap.findWithDefault default_repeat_frames_of_output
                                               argument_repeat_frames_of_output tag_DMap)
---------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
data_iterator :: DMap.Map String String -> [String] -> Int ->  IO ()
data_iterator _ [] _ = return ()
data_iterator tag_DMap (x:rest) i = do
    gnuplot_file tag_DMap >>= \s -> step1 i s x
    data_iterator tag_DMap rest i
    where
       step1 :: Int -> String -> String -> IO ()
       step1 i s x
         |i > 0 = do data_repeater s x
                     step1 (i-1) s x
         |otherwise = return ()
---------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
data_repeater :: String -> String ->  IO ()
data_repeater _ [] = return ()
data_repeater s x  = do putStr $ s ++ x ++ "\nEOF\n"

---------------------------------------------------



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
--------------------------------------------------------





{-- process data when context of data frame is important (Like when processing a time frame).
 ============= And then pass data to gnuplot =======WARNING n_o_t compatible with rest=== --}
data_process_range_sensitive :: DMap.Map String String -> [Int] -> ([a] -> [a]) ->
                                                           (String -> a ) -> (a -> String) -> IO ()
data_process_range_sensitive  tag_DMap [] processor adaptTo adaptFrom = (data_file' $
   data_file tag_DMap) >>= \x -> iterate_all_data tag_DMap $ map adaptFrom $ processor $ map adaptTo x

data_process_range_sensitive tag_DMap range processor adaptTo adaptFrom = (data_file' $
   data_file tag_DMap) >>= \x -> iterate_all_data tag_DMap $ map adaptFrom $ processor $ map adaptTo x

--------------------------------------------------------






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
get_demanded_processors :: String -> [String]
get_demanded_processors arg = words $ map commas2spaces arg
   where
    commas2spaces :: Char -> Char
    commas2spaces c
       |c == ',' = ' '
       |otherwise = c
--------------------------------------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
get_demanded_columns :: String -> (Int, Int)
get_demanded_columns arg = (\x -> (read $ head x, read $ last x) ) $ break_to_columns arg $
                                                                               at_semicolons arg 0
    where
        at_semicolons :: String -> Int -> [Int]
        at_semicolons [] _ = []
        at_semicolons (x:rest) i
           |x == ':' = i:at_semicolons rest (i+1)
           |otherwise = at_semicolons rest (i+1)

        break_to_columns :: String -> [Int] -> [String]
        break_to_columns str [] = [str]
        break_to_columns str (i:rest) = (\(s,sr) -> s : (break_to_columns (tail sr) rest) )
                                                                                    $ splitAt i str

--------------------------------------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
routine::[String] -> IO ()
routine args
  |is_for_test = justtest
  |is_for_bypass = data_bypass tag_DMap' range
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
        |"true" == (DMap.findWithDefault "Not found" argument_test $ tag_DMap') = True
        |otherwise = False


     is_multipage :: Bool
     is_multipage
        |default_multipage_data_file /= (DMap.findWithDefault default_multipage_data_file
                                                 argument_multipage_data_file $ tag_DMap') = True
        |otherwise = False

     is_from_stdin :: Bool
     is_from_stdin
        |default_data_from_stdin /= (DMap.findWithDefault default_data_from_stdin
                                                 argument_data_from_stdin $ tag_DMap') = True
        |otherwise = False

     matrix_stacking_required :: Bool
     matrix_stacking_required
        |default_matrix_stacking /= (DMap.findWithDefault default_matrix_stacking
                                                 argument_matrix_stacking $ tag_DMap') = True
        |otherwise = False


     is_for_bypass :: Bool
     is_for_bypass
        |"true" == (DMap.findWithDefault "Not found" argument_data_bypass_mode $ tag_DMap') = True
        |otherwise = False

-----peculier section , there is processing to do
     there_is_processing :: Bool
     there_is_processing
        |default_data_process /= (DMap.findWithDefault default_data_process argument_data_process
                                                                                 tag_DMap') = True
        |otherwise = False



     do_processing
        |is_multipage && (not matrix_stacking_required)  = multipageDefault
        |is_multipage && matrix_stacking_required      = multipageMatrixStacking

        |is_from_stdin && (not matrix_stacking_required) = stdInDefault
        |is_from_stdin && matrix_stacking_required     = stdInMatrixStacking


        |otherwise = data_processM tag_DMap' range
                       (
                       recognizeDemanded_processors $
                       get_demanded_processors
                       (DMap.findWithDefault default_data_process argument_data_process tag_DMap')
                       )

                       (stringToFloatList_mn_dyn column_m column_n)


     recognizeDemanded_processors :: [String] ->
                           [( [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)] )]
     recognizeDemanded_processors [] = []
     recognizeDemanded_processors proc = step2 proc
        where
        step2 :: [String] -> [( [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)] )]
        step2 [] = []
        step2 (proc:rest)
            |identity_i_processor' proc = identity_i_dyn:(step2 rest)
            |identity_f_processor' proc = identity_f_dyn:(step2 rest)
            |derivative_f_processor' proc = derivative_f_dyn:(step2 rest)
            |derivative_i_processor' proc = derivative_i_dyn:(step2 rest)
            |distance_between_extremums_f_processor' proc = distance_between_extremums_f_dyn:
                                                                                        (step2 rest)
            |extremums_f_processor' proc = extremums_f_dyn:(step2 rest)
            |processor_x_2_f_processor' proc = processor_x_2_f_dyn:(step2 rest)
            |processor_x_2_2_f_processor' proc = processor_x_2_2_f_dyn:(step2 rest)
            |processor_x_2_3_f_processor' proc = processor_x_2_3_f_dyn:(step2 rest)
            |processor_xm_2_f_processor' proc = processor_xm_2_f_dyn:(step2 rest)
            |processor_xm_2_2_f_processor' proc = processor_xm_2_2_f_dyn:(step2 rest)
            |processor_xm_2_3_f_processor' proc = processor_xm_2_3_f_dyn:(step2 rest)
            |otherwise = step2 rest


     multipageDefault = data_processMultipage tag_DMap' range
                       (
                       recognizeDemanded_processors $
                       get_demanded_processors
                       (DMap.findWithDefault default_data_process argument_data_process tag_DMap')
                       )
                       (stringToFloatList_mn_dyn column_m column_n)
                       (weed_data_from_input  $ read
                                                   (DMap.findWithDefault default_multipage_data_file
                                                           argument_multipage_data_file $ tag_DMap')
                       )

     multipageMatrixStacking =  data_processMultipage_matrix_output tag_DMap' range
                       (
                       recognizeDemanded_processors $
                       get_demanded_processors
                       (DMap.findWithDefault default_data_process argument_data_process tag_DMap')
                       )
                       (stringToFloatList_mn_dyn column_m column_n)
                       (weed_data_from_input  $ read
                                                   (DMap.findWithDefault default_multipage_data_file
                                                           argument_multipage_data_file $ tag_DMap')
                       )


     stdInDefault = runEffect $ P.stdinLn >-> (wait_for_piece_of_data []) >->
                     data_processMultipage_fromStdin tag_DMap' range
                       (
                       recognizeDemanded_processors $
                       get_demanded_processors
                       (DMap.findWithDefault default_data_process argument_data_process tag_DMap')
                       )
                       (stringToFloatList_mn_dyn column_m column_n)
                       (weed_data_from_input  $ read
                                                   (DMap.findWithDefault default_data_from_stdin
                                                           argument_data_from_stdin $ tag_DMap')
                       )

     stdInMatrixStacking =
                     runEffect $ P.stdinLn >-> (wait_for_piece_of_data []) >->
                     data_processMultipage_fromStdin_matrix_output tag_DMap' range
                       (
                       recognizeDemanded_processors $
                       get_demanded_processors
                       (DMap.findWithDefault default_data_process argument_data_process tag_DMap')
                       )
                       (stringToFloatList_mn_dyn column_m column_n)
                       (weed_data_from_input  $ read
                                                   (DMap.findWithDefault default_data_from_stdin
                                                           argument_data_from_stdin $ tag_DMap')
                       )

-----end of peculier section


     column_m = (\(x, y) -> x) $ get_demanded_columns
                       (DMap.findWithDefault default_use_columns argument_use_columns tag_DMap')

     column_n = (\(x, y) -> y) $ get_demanded_columns
                       (DMap.findWithDefault default_use_columns argument_use_columns tag_DMap')

     tag_DMap' = tag_DMap args
     range = get_range (DMap.findWithDefault "Not found" argument_range_of_files $ tag_DMap')

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
