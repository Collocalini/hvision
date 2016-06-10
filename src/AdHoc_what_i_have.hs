module AdHoc (

) where



{--
Finally implementing all those drawings i made
--}

import qualified Data.Array.Repa as R
--import qualified Data.Graph as G



data MainContext = MainContext {
   --memory :: Array D (Z :. Integer) Rational
  --,memory_cache :: Array U (Z :. Integer) Rational
  --,index_cache  :: Array U (Z :. Integer) Integer
   memory :: [R.Array R.U (R.Z :. Integer) Rational]
      {-size of each new repa array should be increased exponentially (each 2 times
        bigger then the previous)-}





   }




mainLoop :: Integer -> MainContext Rational




{--
end of drawings
--}

--([[Int]], a)
