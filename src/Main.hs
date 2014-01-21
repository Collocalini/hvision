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
import Text.ParserCombinators.Parsec.Char
--import System.Process
--import Control.Monad


argument_data_file = "data-file"
argument_gnuplot_file = "gnuplot-file"
argument_range_of_files = "range-of-files" -- if range [5..10] then read data5, data6, ... data10
argument_test = "test"
argument_data_bypass_mode = "data-bypass-mode"

default_data_file = "data"
default_gnuplot_file = "plot.gpi"
default_range_of_files = "1..1000" -- if range [5..10] then read data5, data6, ... data10
default_test = "false"
default_data_bypass_mode = "false"

flags = [
         argument_test,
         argument_data_bypass_mode
        ]

options =  [
            argument_data_file ,
            argument_gnuplot_file ,
            argument_range_of_files
           ]


tag_DMap::[String] -> DMap.Map String String
tag_DMap [] = DMap.fromList [
        --("",""),
        (argument_data_file,          default_data_file ),
        (argument_gnuplot_file,       default_gnuplot_file ),
        (argument_range_of_files,     default_range_of_files),
        (argument_test ,              default_test),
        (argument_data_bypass_mode ,  default_data_bypass_mode)
   ]----]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]

tag_DMap lst = DMap.union (DMap.fromList $ map (\(Just x) -> x) $ list_arguments lst) $
                                                                                       tag_DMap []




eol_char = "\n"
eol = try (string $ eol_char) <|>
          (string $ eol_char)



list_arguments :: [String] -> [Maybe (String, String)]
list_arguments [] = []
list_arguments (tag:rest)
  | take 2 tag == "--" && elem tag' flags =
                       (Just (tag', "true")) : list_arguments rest
  | take 2 tag == "--" && elem tag' options =
                       (Just (tag', after_tag)) : list_arguments rest'

  |otherwise = list_arguments rest

  where
     after_tag = head rest
     tag' = (drop 2 tag)

     rest'
        |rest /= [] = tail rest
        |otherwise = []
     rest''
        |rest' /= [] = tail rest'
        |otherwise = []


gnuplot_command = read_file_if_exists "plot.gpi"


read_file_if_exists :: FilePath -> IO String
read_file_if_exists [] = do return ""
read_file_if_exists name  = do
       handle <- openFile name ReadMode
       c <- hGetContents handle
       return c


iterate_all_data :: [IO String] -> IO ()
iterate_all_data [] = putStr ""
iterate_all_data (x:rest) = do
    x >>= \y -> gnuplot_command >>= \s -> putStr $ s ++ y ++ "\nEOF\n"
    iterate_all_data rest


{-- pass data to gnuplot (no processing) --}
data_bypass :: DMap.Map String String -> [Int] -> IO ()
data_bypass  tag_DMap []  = iterate_all_data $
         [read_file_if_exists (DMap.findWithDefault "Not found" ( argument_data_file) $ tag_DMap)]
data_bypass tag_DMap range = iterate_all_data $
           map read_file_if_exists (  map ((DMap.findWithDefault "Not found"
           ( argument_data_file) $ tag_DMap) ++) $ map show range )


get_range :: String -> [Int]
get_range [] = []
get_range range = (\(x,y) -> [(read x)..(read $ drop 2 y)]) $  splitAt
                                               ((\(Just x) -> x) (findIndex (== '.') range)) range

routine::[String] -> IO ()
routine args
  |is_for_test args= justtest
  |is_for_bypass args= data_bypass tag_DMap' range
  |otherwise = putStr ""
   where
   justtest = do  putStrLn "test"


   is_for_test :: [String] -> Bool
   is_for_test str
    |"true" == (DMap.findWithDefault "Not found" ( argument_test) $ tag_DMap') = True
    |otherwise = False

   is_for_bypass :: [String] -> Bool
   is_for_bypass str
    |"true" == (DMap.findWithDefault "Not found" ( argument_data_bypass_mode) $ tag_DMap') =  True
    |otherwise = False

   tag_DMap' = tag_DMap args
   range = get_range (DMap.findWithDefault "Not found" ( argument_range_of_files) $ tag_DMap')




main = do

    getArgs >>= \args -> routine args
    --putStr ""
    --test7x




test7x=do

   --getArgs >>=
  --      (\str -> putStrLn $ show $ tag_DMap str -- $ unwords str
   --     )

   (\(x,y) -> putStrLn ( y) )$  splitAt
                                         ((\(Just x) -> x) (findIndex (== '.') "10..100")) "10..100"

   --putStrLn $ (\(x,y) -> [(read x)..(read $ tail y)]) $  splitAt
   --                                       ((\(Just x) -> x) (findIndex (== '.') "10..100")) "10..100"

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
