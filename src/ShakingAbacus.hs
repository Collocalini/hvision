module ShakingAbacus (
permuteAbac
) where



--main = do
--   putStrLn $ unlines $ map show $ permuteAbac 3 [0,0,1,1,0,0] -- $ take 10 [1..] ---["1","2","3","4","5","6"]



{-- ================================================================================================
===============================================================================================  --}
permuteAbac :: Int -> [a] -> [[a]]--[[[Int]]] --[[a]]
permuteAbac m -- how many to take at a time
               l -- things to select from
               =
   --map (map (\i-> l !! (i-1)) $)$ map reverse $ selectionOrder' m n [1 .. n-(m-1)]
   map ((map (\i-> l !! (i-1)) ).reverse) (selectionOrder' m n [1 .. n-(m-1)])
   where
   n = length l -- how many of things to take from are there
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
===============================================================================================  --}
perMember i m n {-prev-} = concatMap (\(x)-> [[x+1.. n-(m-i)]]
                                     ) {-prev-}
----------------------------------------------------------------------------------------------------
perMember' m n prev
   |i>m = []
   |otherwise = map (: prev) [p+1.. n-(m-i)]
      where
      p = head prev
      i = length prev + 1
----------------------------------------------------------------------------------------------------

{-- ================================================================================================
===============================================================================================  --}
selectionOrder m n l =
   step1 l $ map (\i->perMember i m n) [2..m]
   where
   step1 _ [] = []
   --step1 l (p:rest) = (p $ concat l):(step1 (p $ concat l) rest )
   step1 l (p:rest) = p (concat l):step1 (p $ concat l) rest
----------------------------------------------------------------------------------------------------
selectionOrder' m n l
  -- |null l = l
 --  |i>=20 = l
  -- |otherwise =
    =       step3 step2
          $ concatMap (\prev->perMember' m n [prev]) l
   where
   step1 = map (perMember' m n)

   step2 = replicate (m-2) step1

   step3 [] x = x
   --step3 (s:rest) x  = step3 rest (concat $ s x)
   step3 pms x = foldl (\x s-> concat $ s x) x pms
----------------------------------------------------------------------------------------------------

{-

  [1,      2]
 |    \    |
[2,    3] [3]
 |     |   |
[3,4] [4] [4]


[1,                          2,                3,        4]
 |                           |                 |         |
[2,        3,      4,    5] [3,      4,    5] [4,    5] [5]
 |         |       |     |   |       |     |   |     |   |
[3,4,5,6] [4,5,6] [5,6] [6] [4,5,6] [5,6] [6] [5,6] [6] [6]

-}



