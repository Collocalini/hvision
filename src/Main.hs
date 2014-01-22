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
import qualified Data.Map as DMap
import Text.ParserCombinators.Parsec
--import Text.ParserCombinators.Parsec.Char
--import System.Process
import Control.Monad

---from this project
import Processors
import Global
---end of imports from this project


eol = try (string $ eol_char) <|>
          (string $ eol_char)

gnuplot_file :: DMap.Map String String -> IO String
gnuplot_file tag_DMap = read_file_if_exists (DMap.findWithDefault "Not found"
                                                                 ( argument_gnuplot_file) tag_DMap)
data_file :: DMap.Map String String -> [IO String]
data_file tag_DMap = [read_file_if_exists (DMap.findWithDefault "Not found" ( argument_data_file)
                                                                                         tag_DMap)]
data_file_range :: DMap.Map String String -> [Int] -> [IO String]
data_file_range _ [] = []
data_file_range tag_DMap range = map read_file_if_exists (  map ((DMap.findWithDefault "Not found"
           ( argument_data_file) $ tag_DMap) ++) $ map show range )

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
iterate_all_data' _ [] = putStr ""
iterate_all_data' tag_DMap x  = iterate_all_data tag_DMap x












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
iterate_all_data _ [] = putStr ""
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
routine::[String] -> IO ()
routine args
  |is_for_test = justtest
  |is_for_bypass = data_bypass tag_DMap' range
  |identity_processor = data_process tag_DMap' range identity stringToIntList intListToString
  |derivative_processor = data_process tag_DMap' range derivative stringToIntList intListToString
  |otherwise = putStr ""
     where
     justtest = do  putStrLn "test"


     is_for_test :: Bool
     is_for_test
        |"true" == (DMap.findWithDefault "Not found" argument_test $ tag_DMap') = True
        |otherwise = False

     is_for_bypass :: Bool
     is_for_bypass
        |"true" == (DMap.findWithDefault "Not found" argument_data_bypass_mode $ tag_DMap') = True
        |otherwise = False

     identity_processor :: Bool
     identity_processor
        |"identity" == (DMap.findWithDefault "Not found" argument_data_process $ tag_DMap') = True
        |otherwise = False

     derivative_processor :: Bool
     derivative_processor
        |"derivative" == (DMap.findWithDefault "Not found" argument_data_process $ tag_DMap') = True
        |otherwise = False


     tag_DMap' = tag_DMap args
     range = get_range (DMap.findWithDefault "Not found" argument_range_of_files $ tag_DMap')
-------------------------------------------------------------------------



main = do

    getArgs >>= \args -> routine args
    --putStr ""
    --test8








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

     (data_file' $ data_file_range (tag_DMap str) [1..2]) >>= \x -> putStrLn $ show $ map stringToIntList x    )


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
