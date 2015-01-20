-----------------------------------------------------------------------------
--
-- Module      :  Data_iterators
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

module Data_iterators (
data_file',
iterate_all_data',
gnuplot_file,
data_file,
data_file_range,
get_range,
iterate_all_data,
iterate_all_data_v,
data_iterator,
data_repeater,
IterateData(..)
) where

import qualified Data.Map as DMap
import Global
import qualified Cmd_arguments as CmdA
import System.IO.Unsafe
import Data.List
import Control.Monad.State

data_file'::  IO [String] -> IO [String]
data_file' data_file = data_file


iterate_all_data' :: DMap.Map String String ->  [String] -> IO ()
iterate_all_data' _ [] = return ()
iterate_all_data' tag_DMap x  = iterate_all_data tag_DMap x



gnuplot_file' :: DMap.Map String String -> IO String
gnuplot_file' tag_DMap = --return ""--unsafeInterleaveIO $
                        read_file_if_exists (DMap.findWithDefault "Not found"
                                                                 ( CmdA.argument_gnuplot_file) tag_DMap)

gnuplot_file_v :: FilePath -> StateT IterateData IO String
gnuplot_file_v gpf = lift $ read_file_if_exists gpf



data_file :: DMap.Map String String -> IO [ String]
data_file tag_DMap = sequence [read_file_if_exists (DMap.findWithDefault "Not found"
                                                                               ( CmdA.argument_data_file)
                                                                                         tag_DMap)]
data_file_range :: DMap.Map String String -> [Int] -> IO [ String]
data_file_range _ [] = return []
data_file_range tag_DMap range = unsafeInterleaveMapIO read_file_if_exists $
       map ((DMap.findWithDefault "Not found" ( CmdA.argument_data_file) $ tag_DMap) ++) $ map show range



unsafeInterleaveMapIO f (x:xs) = unsafeInterleaveIO $ do
        y <- f x
        ys <- unsafeInterleaveMapIO f xs
        return (y : ys)
unsafeInterleaveMapIO _ [] = return []




get_range :: String -> [Int]
get_range [] = []
get_range range = (\(x,y) -> [(read x)..(read $ drop 2 y)]) $  splitAt
                                               ((\(Just x) -> x) (findIndex (== '.') range)) range






{-- ================================================================================================
================================================================================================ --}
iterate_all_data :: DMap.Map String String -> [String] ->  IO ()
iterate_all_data _ [] = return ()
iterate_all_data tag_DMap (x:rest)  = do
    data_iterator tag_DMap (x:rest) repeats
    where
        repeats = read $ (DMap.findWithDefault CmdA.default_repeat_frames_of_output
                                               CmdA.argument_repeat_frames_of_output tag_DMap)
---------------------------------------------------


data IterateData = IterateData {
     gnuplot_file :: Maybe FilePath
    ,repeat_frames_of_output :: Maybe Int
     }



{-- ================================================================================================
================================================================================================ --}
iterate_all_data_v ::  [String] -> StateT IterateData IO ()
iterate_all_data_v [] = return ()
iterate_all_data_v (x:rest)  = do
    (IterateData {repeat_frames_of_output = id_rp}) <- get
    case id_rp of
      (Just repeats) -> data_iterator_v (x:rest) repeats
      (Nothing) -> return ()
---------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
data_iterator :: DMap.Map String String -> [String] -> Int ->  IO ()
data_iterator _ [] _ = return ()
data_iterator tag_DMap (x:rest) i = do
    gnuplot_file' tag_DMap >>= \s -> step1 i s x
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
data_iterator_v ::  [String] -> Int -> StateT IterateData IO ()
data_iterator_v [] _ = return ()
data_iterator_v (x:rest) i = do
    (IterateData {gnuplot_file = id_gpf}) <- get
    case id_gpf of
      (Just gpf) -> do
        gnuplot_file_v gpf >>= \s -> return $ step1 i s x
        data_iterator_v rest i
      (Nothing) -> return ()
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







