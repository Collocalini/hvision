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
import Data.List
--import System.Process
--import Control.Monad

data_file = "data"
data_path = "/home/hokum/Documents/pixels_to_signals/sda3/temp/new_Automatic_Traffic_" ++
                                                                    "Surveillance.avi/data_files/"
gnuplot_command = read_file_if_exists "plot.gpi"
--"set terminal jpeg \nplot '-' using 1:2 title \"My Plot 1\" with lp \n"

range = [1..1000]


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
data_bypass = iterate_all_data $
           map read_file_if_exists (  map ((data_path ++ data_file) ++) $ map show range )


main = do

    data_bypass
    --putStr ""

















-- runCommand $ "echo -e " ++ (gnuplot_command ++ x) ++ "|gnuplot -persist"
