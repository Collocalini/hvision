module ShakingAbacus (
 test1
,in4_v_3_v_2
,inN_v_v_v_2
,inN_v_Xrecursive
) where

import ShakingAbacusCommon
import Data.List
import Control.Monad.State


test1 = do
      gnuplot<-readFile "abacus.gpi"
      putStrLn gnuplot
      putStrLn $ unlines $ map unlines
         $ perPermutationSet
         $ map (\(x,y,z)-> (show x) ++ " " ++ (show y) ++ " " ++ [z] ++ " ") $ zip3 xs ys
         $ concat $ concat $ intersperse (replicate singlePermutationSetLength "2")
         $ map singlePermutationSet
         $ map (\(l,r)-> l ++ "1" ++ r) $ zip (map (\x-> replicate x     '0') [0..inputLength-1])
                                              (map (\x-> replicate (inputLength-1-x) '0') [0..inputLength-1])
      putStrLn "EOD"
      where
      inputLength = 4
      selectionLength = 2
      singlePermutationSet = permuteAbac selectionLength
      singlePermutationSetLength = length $ singlePermutationSet [1..inputLength]
      xs = concatMap (replicate singlePermutationSetLength) [1..]
           --(map (\(l,r)-> r+l) $ zip (cycle [0..selectionLength-1])
           -- $ concatMap (replicate (singlePermutationSetLength*selectionLength)) [1,1+selectionLength..])
      ys = cycle [1..singlePermutationSetLength]
           --(cycle $ concatMap (replicate selectionLength) [1..singlePermutationSetLength])
      perPermutationSet [] = []
      perPermutationSet x = (\(l,r)-> l:(perPermutationSet r))
                                  --  $ splitAt (singlePermutationSetLength*selectionLength) x
                                    $ splitAt singlePermutationSetLength x


{- =================================================================================================
В этом тесте я делаю
in=4 -> (3) -> (2)
т.е. вход длинной 4 направляю на счёты длнной выборки 3, затем результаты из 3 последовательно в 2.
Форматирование результата должно быть такое:

----in
-----------------3
----------2
----------2
----------2
----------2
----------2
----------2
================================================================================================= -}


in4_v_3_v_2 = do
      let _in = [1,0,0,0]
      let _3 = permuteAbac 3 _in
      let l3 = length _3
      let l_3 = 3
      let _2 = map (permuteAbac 2) _3
      let l2 = length $ head _2
      let l_2 = 2

      gnuplot<-readFile "abacus.gpi"
      putStrLn gnuplot
      putStrLn $ unlines $ map toStringXYZ $ zip3  [1..4] (replicate 4 1) _in

      putStrLn $ run l_3 l3 6 _3

      putStrLn $ unlines $ map (\(x,f)-> run l_2 l2 x f) $ zip [10,10+3..] _2


      putStrLn "EOF"
      where
      perPermutationSet :: Int -> [a] -> [[a]]
      perPermutationSet _ [] = []
      perPermutationSet spsl x = (\(l,r)-> l:(perPermutationSet spsl r))
                                    $ splitAt spsl x

      toStringXYZ (x,y,z) = (show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " "

      run :: Show a => Int -> Int -> Int -> [[a]] -> String
      run  l_1 l1 x _1 = unlines $ map unlines  $ perPermutationSet (l_1)
         $ map toStringXYZ $ zip3 xs ys $ concat _1
         where
         xs = cycle [x..x+l_1-1]
         ys = concat $ map (replicate (l_1)) [1..l1]

----------------------------------------------------------------------------------------------------


{-
-}

--newtype AbacPerm a = [[a]]

data InN_v_v_v_2 = InN_v_v_v_2 {
    startXinRun :: Int
   ,inputRank   :: Int
   ,output      :: [String]
   ,input       :: [[Int]]
   }

inN_v_v_v_2 _in gpi = do
   let ln = length _in
   gnuplot<-readFile gpi
   putStrLn gnuplot
   putStrLn $ unlines $ map toStringXYZ $ zip3  [1..length _in] (replicate (length _in) 1) _in

   putStrLn $ evalState runN $ InN_v_v_v_2 {
                                  startXinRun = ln+2
                                 ,inputRank   = ln-1
                                 ,output      = []
                                 ,input       = [_in]
                                 }

   putStrLn "EOF"
   where
      perPermutationSet :: Int -> [a] -> [[a]]
      perPermutationSet _ [] = []
      perPermutationSet spsl x = (\(l,r)-> l:(perPermutationSet spsl r))
                                    $ splitAt spsl x

      toStringXYZ (x,y,z) = (show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " "

      run :: Show a => Int -> Int -> Int -> [[a]] -> String
      run  l_1 l1 x _1 = unlines $ map unlines  $ perPermutationSet (l_1)
         $ map toStringXYZ $ zip3 xs ys $ concat _1
         where
         xs = cycle [x..x+l_1-1]
         ys = concat $ map (replicate (l_1)) [1..l1]

      runN :: State InN_v_v_v_2 String
      runN = do
         InN_v_v_v_2 { startXinRun = x
                      ,inputRank   = ir
                      ,output      = output
                      ,input       = input} <-get

         let ip = map (permuteAbac ir) input
         let op = unlines $ map (\(x,f)-> run ir (length $ head ip) x f) $ zip [x,x+ir+1..] ip

         case (ir>1) of
            True -> do
               put $ InN_v_v_v_2 {
                                  startXinRun = x+((ir+1) * (length ip))+2
                                 ,inputRank   = ir-1
                                 ,output      = (op:output)
                                 ,input       = concat ip
                                 }
               runN

            False -> do return $ concat $ reverse (op:output)




inN_v_Xrecursive _in gpi iterations ir = do
   let ln = length _in
   gnuplot<-readFile gpi
   putStrLn gnuplot
   putStrLn $ unlines $ map toStringXYZ $ zip3  [1..length _in] (replicate (length _in) 1) _in

   putStrLn $ evalState (runN iterations) $ InN_v_v_v_2 {
                                  startXinRun = ln+2
                                 ,inputRank   = ir
                                 ,output      = []
                                 ,input       = [_in]
                                 }

   putStrLn "EOF"
   where
      perPermutationSet :: Int -> [a] -> [[a]]
      perPermutationSet _ [] = []
      perPermutationSet spsl x = (\(l,r)-> l:(perPermutationSet spsl r))
                                    $ splitAt spsl x

      toStringXYZ (x,y,z) = (show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " "

      run :: Show a => Int -> Int -> Int -> [[a]] -> String
      run  l_1 l1 x _1 = unlines $ map unlines  $ perPermutationSet (l_1)
         $ map toStringXYZ $ zip3 xs ys $ concat _1
         where
         xs = cycle [x..x+l_1-1]
         ys = concat $ map (replicate (l_1)) [1..l1]

      runN :: Int -> State InN_v_v_v_2 String
      runN i = do
         InN_v_v_v_2 { startXinRun = x
                      ,inputRank   = ir
                      ,output      = output
                      ,input       = input} <-get

         let ip = permuteAbac ir $ concat input
         let op = run ir (length $ head ip) x ip

         case (i>1) of
            True -> do
               put $ InN_v_v_v_2 {
                                  startXinRun = x+ir+2
                                 ,inputRank   = ir
                                 ,output      = (op:output)
                                 ,input       = ip
                                 }
               runN $ i-1

            False -> do return $ concat $ reverse (op:output)












