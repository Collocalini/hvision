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
read_file_if_exists :: FilePath -> IO String
read_file_if_exists [] = do return ""
read_file_if_exists name  = do
       handle <- openFile name ReadMode
       c <- hGetContents handle
       return c
-------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
iterate_all_data :: DMap.Map String String -> [String] ->  IO ()
iterate_all_data _ [] = return ()
iterate_all_data tag_DMap (x:rest)  = do
    gnuplot_file tag_DMap >>= \s -> putStr $ s ++ x ++ "\nEOF\n"
    iterate_all_data tag_DMap rest
---------------------------------------------------




{-- pass data to gnuplot (no processing) =========================================================
================================================================================================ --}
data_bypass :: DMap.Map String String -> [Int] -> IO ()
data_bypass  tag_DMap []  =  (data_file' $ data_file tag_DMap) >>= \x -> iterate_all_data tag_DMap x
data_bypass tag_DMap range = (data_file' $ data_file_range tag_DMap range) >>= \x ->
                                                                       iterate_all_data tag_DMap x

--------------------------------------------------------





{-- process and pass data to gnuplot +=============================================================
================================================================================================ --}
data_process :: DMap.Map String String -> [Int] -> ([a] -> [a]) ->
                                              (String -> [a] ) -> ([a] -> String) -> IO ()
data_process  tag_DMap [] processor adaptTo adaptFrom = (data_file' $ data_file tag_DMap) >>= \x ->
                                    iterate_all_data tag_DMap $ map (adaptFrom . processor . adaptTo) x


data_process tag_DMap range processor adaptTo adaptFrom = (data_file' $
   data_file_range tag_DMap range) >>= \x ->
                                    iterate_all_data tag_DMap $ map (adaptFrom . processor . adaptTo) x

--------------------------------------------------------





{-- process and pass data to gnuplot +=============================================================
================================================================================================ --}

data_processM :: DMap.Map String String -> [Int] ->
                                [([(Dynamic, Dynamic)] -> [ (Processor_data, Processor_data) ])] ->
                                (String -> [(Dynamic, Dynamic)] ) ->
                                IO ()
data_processM  tag_DMap [] processors adaptTo =
    (data_file' $ data_file tag_DMap) >>= \x -> iterate_all_data tag_DMap $
                    map (toStringTable . stack_output . (apply_processors (processors)) . adaptTo) x

data_processM tag_DMap range processors adaptTo = (data_file' $
   data_file_range tag_DMap range) >>= \x -> iterate_all_data tag_DMap $
                    map (toStringTable . stack_output . (apply_processors (processors)) . adaptTo) x
--------------------------------------------------------





{-- process and pass data to gnuplot +=============================================================
================================================================================================ --}

data_processMultipage :: DMap.Map String String -> [Int] ->
                                [([(Dynamic, Dynamic)] -> [ (Processor_data, Processor_data) ])] ->
                                (String -> [(Dynamic, Dynamic)] ) ->
                                ([String] -> [String]) ->
                                IO ()
data_processMultipage  tag_DMap [] processors adaptTo prepare_input =
    (data_file' $ data_file tag_DMap) >>= \x -> iterate_all_data tag_DMap $
         map (toStringTable . stack_output . (apply_processors (processors)) . adaptTo) $ prepare_input x

data_processMultipage tag_DMap range processors adaptTo prepare_input = (data_file' $
   data_file_range tag_DMap range) >>= \x -> iterate_all_data tag_DMap $
         map (toStringTable . stack_output . (apply_processors (processors)) . adaptTo) $ prepare_input x
--------------------------------------------------------





{-- process data when context of data frame is important (Like when processing a time frame).
 ============= And then pass data to gnuplot ================================================== --}
data_process_range_sensitive :: DMap.Map String String -> [Int] -> ([a] -> [a]) ->
                                                           (String -> a ) -> (a -> String) -> IO ()
data_process_range_sensitive  tag_DMap [] processor adaptTo adaptFrom = (data_file' $
   data_file tag_DMap) >>= \x -> iterate_all_data tag_DMap $ map adaptFrom $ processor $ map adaptTo x

data_process_range_sensitive tag_DMap range processor adaptTo adaptFrom = (data_file' $
   data_file tag_DMap) >>= \x -> iterate_all_data tag_DMap $ map adaptFrom $ processor $ map adaptTo x

--------------------------------------------------------






{-- ================================================================================================
================================================================================================ --}
get_demanded_processors :: String -> [String]
get_demanded_processors arg = break_to_processors arg $ at_commas arg 0
    where
        at_commas :: String -> Int -> [Int]
        at_commas [] _ = []
        at_commas (x:rest) i
           |x == ',' = i:at_commas rest (i+1)
           |otherwise = at_commas rest (i+1)

        break_to_processors :: String -> [Int] -> [String]
        break_to_processors str [] = [str]
        break_to_processors str (i:rest) = (\(s,sr) -> s : (break_to_processors (tail sr) rest) )
                                                                                    $ splitAt i str

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
        |is_multipage = data_processMultipage tag_DMap' range
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

        |otherwise = data_processM tag_DMap' range
                       (
                       recognizeDemanded_processors $
                       get_demanded_processors
                       (DMap.findWithDefault default_data_process argument_data_process tag_DMap')
                       )

                       --(stringToIntList_dyn)
                       --(stringToIntList_mn_dyn 1 2)
                       (stringToFloatList_mn_dyn column_m column_n)


     recognizeDemanded_processors :: [String] ->
                           [( [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)] )]
     recognizeDemanded_processors proc = map ((\(Just x) -> x) . step1) proc
       where
        step1 :: String -> Maybe ( [(Dynamic, Dynamic)] -> [(Processor_data, Processor_data)] )
        step1 proc
            |identity_i_processor' proc = Just identity_i_dyn
            |identity_f_processor' proc = Just identity_f_dyn
            |derivative_f_processor' proc = Just derivative_f_dyn
            |derivative_i_processor' proc = Just derivative_i_dyn

            |otherwise = Nothing




-----end of peculier section


     column_m = (\(x, y) -> x) $ get_demanded_columns
                       (DMap.findWithDefault default_use_columns argument_use_columns tag_DMap')

     column_n = (\(x, y) -> y) $ get_demanded_columns
                       (DMap.findWithDefault default_use_columns argument_use_columns tag_DMap')

     tag_DMap' = tag_DMap args
     range = get_range (DMap.findWithDefault "Not found" argument_range_of_files $ tag_DMap')

----------------------------------------------------------------------------------------------------






stdinLn :: Producer String IO ()
stdinLn = do
     eof <- lift isEOF        -- 'lift' an 'IO' action from the base monad
     unless eof $ do
         str <- lift getLine
         yield str            -- 'yield' the 'String'
         stdinLn              -- Loop






main = do

    getArgs >>= \args -> routine args
    --putStr ""
    --test11





test12 = do ---return ()
   runEffect $ for stdinLn (lift . putStrLn . ("-----" ++))






test11 = do
   getArgs >>=
     (\str -> -- $ unwords str


     (data_file' $ data_file $ tag_DMap str) >>= \x -> iterate_all_data (tag_DMap str) $
        {-- map (toStringTable . stack_output . (apply_processors ([(identity_f_dyn)])) .
                                       stringToFloatList_mn_dyn 1 2) $--} weed_data_from_input 3 x
     )


     --(data_file' $ data_file (tag_DMap str)) >>=
     --                       \x -> putStrLn $ show $ weed_data_from_input 3  $
     --                                                              stringToFloatList_mn_dyn 1 2 x)




test10  = do
    putStr $ show $ weed_data_from_input 3
      [" 1 \n 2 \n 3 \n 4 \n x \n x \n EOF \n 5 \n 6 \n 7 \n 8 \n EOF \n 9 \n 10 \n" ++
        " 11 \n 12 \n 13 \n 14 \n",
       " 1 \n 2 \n 3 \n 4 \n x \n x \n EOF \n 5 \n 6 \n 7 \n 8 \n EOF \n9 \n 10 \n" ++
        " 11 \n 12 \n 13 \n 14 \n",
       " 1 \n 2 \n 3 \n 4 \n x \n x \n EOF \n 5 \n 6 \n 7 \n 8 \n EOF \n9 \n 10 \n" ++
        " 11 \n 12 \n 13 \n 14 \n"
      ]





test9 = do
     --putStrLn $ show $ map (\(x, y) -> ((fromDyn x 0::Int), (fromDyn y 0::Int)) )  $ head $
     --      apply_processors [(identity_i_dyn)] [(toDyn (1::Int), toDyn (2::Int))]

     putStrLn ${-- show $ (\x-> map (\(Pd y _) -> y) x)--}toStringTable $ stack_output $
                           apply_processors [(identity_i_dyn),
                                             (identity_f_dyn),
                                             (derivative_i_dyn),
                                             (derivative_f_dyn)
                                            ]                  [(toDyn (1::Int), toDyn (101::Int)),
                                                                (toDyn (2::Int), toDyn (102::Int)),
                                                                (toDyn (3::Int), toDyn (103::Int)),
                                                                (toDyn (4::Int), toDyn (104::Int)),
                                                                (toDyn (5::Int), toDyn (105::Int)),
                                                                (toDyn (6::Int), toDyn (106::Int)),
                                                                (toDyn (7::Int), toDyn (107::Int)),
                                                                (toDyn (8::Int), toDyn (108::Int)),
                                                                (toDyn (9::Int), toDyn (109::Int)),
                                                                (toDyn (10::Int), toDyn (110::Int))
                                                               ]



test8 = do

  getArgs >>=
     (\str -> putStrLn $ show $ tag_DMap str -- $ unwords str
     )

  --getArgs >>=
   --  (\str -> -- $ unwords str

    -- data_process (tag_DMap str) [1..100] identity ioStringToIntList intListToIoString
     --)

  getArgs >>=
     (\str -> -- $ unwords str

     (data_file' $ data_file_range (tag_DMap str) [1..2]) >>=
                            \x -> putStrLn $ show $ map stringToIntList x    )


test7x=do

   --getArgs >>=
  --      (\str -> putStrLn $ show $ tag_DMap str -- $ unwords str
   --     )

 (\(x,y) -> putStrLn ( y) )$  splitAt
                                         ((\(Just x) -> x) (findIndex (== '.') "10..100")) "10..100"

   --putStrLn $ (\(x,y) -> [(read x)..(read $ tail y)]) $  splitAt
   --                                     ((\(Just x) -> x) (findIndex (== '.') "10..100")) "10..100"

test7=do
  getArgs >>=
    (\str -> putStrLn $ show $ str
    )

  getArgs >>=
    (\str -> putStrLn $ show $ tag_DMap [] -- $ unwords str
    )


--
  getArgs >>=
    (\str -> putStrLn $ show $ list_arguments $
              str
    )

  getArgs >>=
    (\str -> putStrLn $ show $
              (DMap.member "test" $ tag_DMap []) --["test", "test"]--flags
    )

  getArgs >>=
    (\str -> putStrLn $ show $
              take 2 "--data-file"  == "--" && elem (drop 2 "--data-file") options
    )











-- runCommand $ "echo -e " ++ (gnuplot_command ++ x) ++ "|gnuplot -persist"
