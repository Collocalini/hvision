
module Processors (

identity_i,
identity_f,
derivative_i,
derivative_f,

stringToIntList,
intListToString,
stringToFloatList,
floatListToString,

stringToIntList_mn,
stringToFloatList_mn,
intListToString_2to3,
floatListToString_2to3

) where


eol_char = "\n"


------------------------------------ section of processors -----------------------------------------



{-- ================================================================================================
================================================================================================ --}
identity_i :: [(Int, Int)] -> [(Int, Int)]
identity_i  row = row
---------------------------------




{-- ================================================================================================
================================================================================================ --}
identity_f :: [(Float, Float)] -> [(Float, Float)]
identity_f  row = row
---------------------------------




{-- ================================================================================================
================================================================================================ --}


derivative_i :: [(Int, Int)] -> [(Int, Int)]
derivative_i  [] = []
derivative_i  row@(prev@(x_prev, y_prev):x_rest) = (x_prev, 0):(step1 row)
     where
        step1 :: [(Int, Int)] -> [(Int, Int)]
        step1  [] = []
        step1 (prev@(x_prev, y_prev):x_rest) = ((\(x, y) -> (x , y-y_prev) ) $ head x_rest):
             (step1 $ tail x_rest)



derivative_f :: [(Float, Float)] -> [(Float, Float)]
derivative_f  [] = []
derivative_f  row@(prev@(x_prev, y_prev):x_rest) = (x_prev, 0):(step1 row)
     where
        step1 :: [(Float, Float)] -> [(Float, Float)]
        step1  [] = []
        step1 (prev@(x_prev, y_prev):x_rest) = ((\(x, y) -> (x_prev + ( (abs $ x - x_prev)/2),
                                                  y-y_prev) ) $ head x_rest):(step1 $ tail x_rest)
---------------------------------




-------------------------------- end of section of processors --------------------------------------



------------------read columns 1,2 of (1..inf)--------------------------------------------------------
{-- ================================================================================================
================================================================================================ --}
stringToIntList :: String -> [(Int, Int)]
stringToIntList str =  map step1 $ lines str
  where
  step1 :: String -> (Int, Int)
  step1 str = (\x -> (read $ head x , read $ head $ tail x) ) $ words str
------------------------------

{-- ================================================================================================
================================================================================================ --}
stringToFloatList :: String -> [(Float, Float)]
stringToFloatList str =  map step1 $ lines str
  where
  step1 :: String -> (Float, Float)
  step1 str = (\x -> (read $ head x , read $ head $ tail x) ) $ words str
------------------------------

------------------end of ---- read columns 1,2 of (1..inf)------------------------------------------







------------------read columns m,n of (1..inf)-----------------------------------------------------
{-- ================================================================================================
================================================================================================ --}
stringToIntList_mn :: Int -> Int -> String ->  [(Int, Int)]
stringToIntList_mn m n str =  map (step1 m n) $ lines str
  where
  step1 :: Int -> Int -> String ->   (Int, Int)
  step1 m n str = (\x -> (read $ head $ drop (m-1) x , read $ head $ drop (n-1) x) ) $ words str
------------------------------

{-- ================================================================================================
================================================================================================ --}
stringToFloatList_mn :: Int -> Int -> String ->  [(Float, Float)]
stringToFloatList_mn m n str =  map (step1 m n) $ lines str
  where
  step1 :: Int -> Int -> String ->  (Float, Float)
  step1 m n str = (\x -> (read $ head $ drop (m-1) x , read $ head $ drop (n-1) x) ) $ words str
------------------------------

------------------end of ---- read columns m,n of (1..inf)------------------------------------------









------------------ put 2 columns back as string ----------------------------------------------------
{-- ================================================================================================
================================================================================================ --}
intListToString :: [(Int, Int)] -> String
intListToString x = concat $ map (\(x,y) -> (show x) ++ " " ++ (show y) ++ eol_char ) x
---------------------------------


{-- ================================================================================================
================================================================================================ --}
floatListToString :: [(Float, Float)] -> String
floatListToString x = concat $ map (\(x,y) -> (show x) ++ " " ++ (show y) ++ eol_char ) x
---------------------------------

------------------ end of ------ put 2 columns back as string -------------------------------------







------------------ put 2 graphs as 3 columns ----------------------------------------------------
step2to3 :: (a, b) -> (c, d) -> (a, b, d)
step2to3 (a, b) (c, d) = (a, b, d)

{-- ================================================================================================
================================================================================================ --}
intListToString_2to3 :: [(Int, Int)] -> [(Int, Int)] -> String
intListToString_2to3 y y1 = concat $ map
        (\(x,w,z) -> (show x) ++ " " ++ (show w) ++ " " ++ (show z) ++ eol_char ) $ zipWith step2to3 y y1

---------------------------------


{-- ================================================================================================
================================================================================================ --}
floatListToString_2to3 :: [(Float, Float)] -> [(Float, Float)] -> String
floatListToString_2to3 y y1 = concat $ map
        (\(x,w,z) -> (show x) ++ " " ++ (show w) ++ " " ++ (show z) ++ eol_char ) $ zipWith step2to3 y y1
---------------------------------

------------------ end of ------ put 2 graphs as 3 columns -------------------------------------





------------------ put n graphs as m columns ----------------------------------------------------

toString_zip :: [String] -> [String] -> [String]
toString_zip x y = map (\(x,y) -> x ++ " " ++ " " ++ y ) $ zip x y


xy2string_ii :: [(Int, Int)] -> ([String] , [String])
xy2string_ii x = unzip $ map (\(x,y) -> (show x, show y) ) x

xy2string_if :: [(Int, Float)] -> ([String] , [String])
xy2string_if x = unzip $ map (\(x,y) -> (show x, show y) ) x

xy2string_ff :: [(Float, Float)] -> ([String] , [String])
xy2string_ff x = unzip $ map (\(x,y) -> (show x, show y) ) x

xy2string_fi :: [(Float, Int)] -> ([String] , [String])
xy2string_fi x = unzip $ map (\(x,y) -> (show x, show y) ) x

------------------ end of ------ put n graphs as m columns -------------------------------------











