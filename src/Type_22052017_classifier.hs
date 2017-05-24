
--{-# LANGUAGE OverloadedStrings #-} 
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Type_22052017_classifier (
 step
,sink
,readFile_tc
,Vertex(..)
,Input(..)
,A
,Context(..)
,BudgetForActions(..)
) where


import Data.Sequence
import Algebra.Graph.AdjacencyMap
import Control.Lens hiding ((.=),Context)
import qualified Control.Lens as L --hiding (element)
import Data.DList hiding (map)
import qualified Data.DList as DL
import Data.Maybe
import Data.Word
import Control.Monad.RWS.Lazy
import Control.Monad.Except
import Data.Conduit
import Data.Conduit.List hiding (map)
import qualified Data.Conduit.List as Cl




data VertexLabel = BwaaMore
                  |BwaaLess
                  |BwaaSame
                  |MoreMeta
                  |LessMeta
                  |SameMeta
                  |Prosvet Word
                        deriving (Show,Eq)

data Neighbour   = N { _nnumber :: Word
                      ,_labels :: Seq VertexLabel
                     }deriving (Show)

instance Eq Neighbour where
    (N{_nnumber=n1}) == (N{_nnumber=n2}) = n1 == n2

L.makeLenses ''Neighbour

--Data Graph = Seq Vertex
data Vertex = V 
     { _value  :: Word8  -- value
      ,_vnumber :: Word  -- generation counter. Vertexes are created sequentially and this is number in sequence of creation
      ,_metric :: Word
      ,_neighbours :: Seq Neighbour
     } deriving (Show,Eq)

instance Ord Vertex where
    compare (V{_vnumber=v1}) (V{_vnumber=v2}) = compare v1 v2


L.makeLenses ''Vertex


data BudgetForActions = 
   B { _calls :: Word
      ,_memory :: Word
     }deriving (Show)
L.makeLenses ''BudgetForActions


data Context = 
   C { _graph :: AdjacencyMap Vertex
      ,_entrancesLow  :: Seq Vertex
      ,_entrancesHigh :: Seq Vertex
     -- ,_freeCells     :: Seq Int
     -- ,_maxCells      :: Word
      ,_budget :: BudgetForActions
     }deriving (Show)
L.makeLenses ''Context







data ApplicationLog = 
    RanOutOfCalls    String
   |RanOutOfMemory   String
   |FunctionComplete String
   |OtherError       String 
   deriving (Show)


data Input = I {
    _input :: Maybe Word8
   ,_think :: Bool
   ,_answer :: Bool
   }deriving (Show)
L.makeLenses ''Input

type A' m = ExceptT ApplicationLog (RWST BudgetForActions (DList String) Context m) 
type A = A' IO


readFile_tc :: String -> Source A Input
readFile_tc n = do
   f <- liftIO $ readFile n
   sourceList $ map x $ map words $ lines f
   where
    x [i, t, a] = I { _input = Just $ read i
                     ,_think = read t
                     ,_answer = read a
                    }
    x _         = I { _input = Nothing
                     ,_think = False
                     ,_answer = False
                    }


sink :: Sink Word8 A [Word8]
sink = consume

step :: Conduit Input A Word8
step = do
   ar <- await
   case ar of
    Nothing -> return ()
    Just a -> do
      
      yield (fromMaybe 0 $ a^.input)
      step



{--
addEdge :: V -> V -> (V, V)
addEdge v1 v2 = v1^.neighbours

makeRow :: Graph --vertexes to be connected into a row
        -> Graph --row
makeRow vertexes = 


attachRowToCorresponding :: Graph  --original graph
                         -> Graph  --Row
                         -> Graph  --Resulting Graph
--}















