module AdHoc (

) where



{--
Finally implementing all those drawings i made
--}

import qualified Data.Array.Repa as R
--import qualified Data.Graph as G
import Data.Conduit
import Control.Monad.State



type Graph = Maybe (R.Array R.U (R.Z :. Integer) GraphCell)
type Memory = R.Array R.D (R.Z :. Integer) Cell




data Cell = Cell {
    value :: Maybe Rational
 --  ,
   }



data GraphCell = Cell {
    value :: Maybe Rational
   ,neighbors :: Maybe (R.Array R.D (R.Z :. Integer) Integer)
   }




data MainContext = MainContext {

    memory :: Memory
      {-size of each new repa array should be increased exponentially (each 2 times
        bigger then the previous)-}

   --,input  :: Graph
   ---,output :: Graph



   }



--Sink Graph (State MainContext IO) ()

--getInput :: StateT MainContext IO


{--From various formats into Graph--}
sense :: Source (StateT MainContext IO) Graph


{--Manage memory issues.
   Sorting and caching for fast access is happening here
   --}
remember :: Conduit (StateT MainContext IO) Memory

{--Extrapolate memories using predefined heuristics.
   This is the step where only exact resolutions(конкретные воспоминания) of memories are formed.
   --}
dream :: Conduit (StateT MainContext IO) Memory

{--Form abstractions(обобщения) over memories.
   Different convolutions(свёртки) are happening here.
   --}
think :: Conduit (StateT MainContext IO) Memory

{--Use memories to form an answer.
   An answer is a piece of memory consisting of a number of cells activated by sences.
   --}
say :: Sink Memory (StateT MainContext IO) Graph




theAIExperiment :: MainContext -> IO
theAIExperiment mc = do
   runStateT mainLoop mc



{-mainLoopIO :: StateT MainContext IO
mainLoopIO = do
   getInput
   mainLoop
-}

mainLoop :: StateT MainContext IO Graph
mainLoop mc = sense $= remember =$= dream =$= think =$ say







{--
end of drawings
--}

--([[Int]], a)
