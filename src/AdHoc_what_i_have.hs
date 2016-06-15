module AdHoc (

) where



{--
Finally implementing all those drawings i made
--}

import qualified Data.Array.Repa as R
--import qualified Data.Graph as G
import Data.Conduit
import qualified Data.Sequence as Seq
import Control.Monad.State



type Graph = Maybe (R.Array R.U (R.Z :. Integer) GraphCell)


type Memory = R.Array R.D (R.Z :. Integer) Cell
   {-delayed internal representation-}

data MemoryInternal = MemoryInternal
   {
    memory :: Seq.Seq Cell --Seq.Seq (R.Array R.U (R.Z :. Integer) Cell)
   {-size of each new repa array should be increased exponentially (each 2 times
        bigger then the previous). Initial size is 2. [2, 4, 8, 16 ..]-}

   ,missedHits :: Seq.Seq CellAdress
   ,hopsTillFree :: Seq.Seq (Seq.Seq CellAdress)
   ,head :: CellAdress
   }



data CellAdress = CellAdress
   {
    dli :: Integer --delayed linear index
  -- ,ii1 :: Integer --index in seq, aka level 1
  -- ,ii2 :: Integer --index in repa, aka level 2
   }


data Cell = Cell {
    value :: Maybe Rational
   ,neighbors :: Seq.Seq CellAdress
   ,abstractNeighbors :: Seq.Seq CellAdress
   ,dreamNeighbors :: Seq.Seq CellAdress
   ,maybeVacant :: Seq.Seq CellAdress
   ,meInRecentWalks :: Seq.Seq CellAdress
   }


emptyCell = Cell {
    value             = Nothing
   ,neighbors         = Seq.empty
   ,abstractNeighbors = Seq.empty
   ,dreamNeighbors    = Seq.empty
   ,maybeVacant       = Seq.empty
   ,meInRecentWalks   = Seq.empty
   }




data GraphCell = GraphCell {
    value :: Maybe Rational
   ,neighbors :: Maybe (R.Array R.D (R.Z :. Integer) Integer)
   }




data MainContext = MainContext {

    memory :: MemoryInternal


   ,input  :: Maybe Graph
   ,output :: Maybe Graph



   }



--Sink Graph (State MainContext IO) ()

--getInput :: StateT MainContext IO


{--From various formats into Graph--}
sense :: Source (StateT MainContext IO) Graph





{--Manage memory issues.
   Sorting and caching for fast access is happening here
   --} --Conduit Graph (StateT MainContext IO) ()
remember :: State MainContext ()
--remember = do


{--Extrapolate memories using predefined heuristics.
   This is the step where only exact resolutions(конкретные воспоминания) of memories are formed.
   --}
dream :: Conduit () (StateT MainContext IO) ()



{--Form abstractions(обобщения) over memories.
   Different convolutions(свёртки) are happening here.
   --}
think :: Conduit () (StateT MainContext IO) ()




compose :: State MainContext ()




decompose :: State MainContext ()



hypothesis :: State MainContext ()




trainOfThought :: State MainContext ()







{--Use memories to form an answer.
   An answer is a piece of memory consisting of a number of cells activated by sences.
   --}
say :: Sink () (StateT MainContext IO) Graph



{-

memIndex
   :: Integer --index(in delayed repa)
   -- -> Integer --length of sequence
   -> (Integer, Integer) -- index in seq, index in repa
memIndex i {-l-} = (iInSeq-1, iInRePA-1)
   where
   --lengthOfIthRePA i = 2^i
   iInSeq  = truncate $ logBase 2 (i+1)
   iInRePA = i-( 2^(iInSeq-1) )






readMemory
   :: Integer --index(in delayed repa)
 --  -> MemoryInternal
   -> State MemoryInternal Cell
readMemory i = do return $ whenValid $ validIndex i
   where

   whenValid (Just (_,_,_,c)) = c
   whenValid Nothing          = returnNothing

   returnNothing = (\c -> c {value = Nothing}) emptyCell







validIndex
   :: Integer --index(in delayed repa)
  -- -> MemoryInternal
   -> State MemoryInternal
        (
         Maybe (  Integer          --index(in delayed repa) the same as the input, unchanged
                , Integer, Integer --index in seq, index in repa
                , Cell)            --and the value while we are at it
        )
validIndex i = do
   m <- get
   return $ validIndex_plain i m








validIndex_plain
   :: Integer --index(in delayed repa)
   -> MemoryInternal
   -> Maybe (  Integer          --index(in delayed repa) the same as the input, unchanged
             , Integer, Integer --index in seq, index in repa
             , Cell)            --and the value while we are at it

validIndex_plain i m
   |is < (length m) = when_is_Fits $ Seq.index m is
   |otherwise       = Nothing
   where
   (is, ia) = memIndex i

   when_is_Fits a
      |ia < (size $ extent a) = Just (i, is, ia, a ! (R.Z :. ia))
      |otherwise              = Nothing







writeToMemory
   :: Cell
   -> State MemoryInternal ()
writeToMemory c = do


-}




theAIExperiment :: MainContext -> IO
theAIExperiment mc = do
   runStateT mainLoop mc



{-mainLoopIO :: StateT MainContext IO
mainLoopIO = do
   getInput
   mainLoop
-}

mainLoop :: StateT MainContext IO Graph
mainLoop mc = --sense $= remember =$= dream =$= think =$ say







{--
end of drawings
--}

--([[Int]], a)







{--
import qualified Data.Array.Repa as R
--import qualified Data.Graph as G
--import Data.Conduit
import qualified Data.Sequence as Seq
import Control.Monad.State



memIndex
   :: Integer --index(in delayed repa)
   -- -> Integer --length of sequence
   -> (Integer, Integer) -- index in seq, index in repa
memIndex i {-l-} = (iInSeq-1, iInRePA-1)
   where
   --lengthOfIthRePA i = 2^i
   iInSeq  = truncate $ logBase 2 (i+1)
   iInRePA = i-( 2^(iInSeq-1) )







readMemory
   :: Integer --index(in delayed repa)
 --  -> MemoryInternal
   -> State MemoryInternal Cell
readMemory i = do return $ whenValid $ validIndex i
   where

   whenValid (Just (_,_,_,c)) = c
   whenValid Nothing          = returnNothing

   returnNothing = (\c -> c {value = Nothing}) emptyCell







validIndex
   :: Integer --index(in delayed repa)
  -- -> MemoryInternal
   -> State MemoryInternal
        (
         Maybe (  Integer          --index(in delayed repa) the same as the input, unchanged
                , Integer, Integer --index in seq, index in repa
                , Cell)            --and the value while we are at it
        )
validIndex i = do
   m <- get
   return $ validIndex_plain i m








validIndex_plain
   :: Integer --index(in delayed repa)
   -> MemoryInternal
   -> Maybe (  Integer          --index(in delayed repa) the same as the input, unchanged
             , Integer, Integer --index in seq, index in repa
             , Cell)            --and the value while we are at it

validIndex_plain i m
   |is < (length m) = when_is_Fits $ Seq.index m is
   |otherwise       = Nothing
   where
   (is, ia) = memIndex i

   when_is_Fits a
      |ia < (size $ extent a) = Just (i, is, ia, a ! (R.Z :. ia))
      |otherwise              = Nothing






run :: StateT MainContext IO
run =do
   readMemory 5




main = do
   runStateT run


--}










