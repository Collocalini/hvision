
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
,Tick(..)
,BudgetForActions(..)
) where


import Data.Sequence hiding (length,empty,fromList,null,zip,filter,lookup)
import qualified Data.Sequence as Sq
import Data.Set hiding (map,filter,empty,fromList,null,foldl)
import qualified Data.Set as S
import Algebra.Graph.AdjacencyMap hiding (empty,graph)
import qualified Algebra.Graph.AdjacencyMap as Ag
import Control.Lens hiding ((.=),Context,(|>))
import qualified Control.Lens as L --hiding (element)
import Data.DList hiding (map,empty,fromList,head,concat,lookup)
import qualified Data.DList as DL
import Data.Maybe
import Data.Word
import Control.Monad.RWS.Lazy
import Control.Monad.Except
import Data.Conduit
import Data.Conduit.List hiding (map,mapM,head,concatMap,concat,filter,lookup)
import qualified Data.Conduit.List as Cl





data Tick = Tick Integer deriving (Show)


--data Correction = CPositive|CNegative

data VertexLabel = BwaaMore
                  |BwaaLess
                  |BwaaSame
                  |MoreMeta
                  |LessMeta
                  |SameMeta
                  |Prediction
                  |Correction
                  |CPositive
                  |CNegative
                  |Input
                  |Same
                  |Forward
                  |Backward
                  |Prosvet Word
                        deriving (Show,Eq,Ord)

data Neighbour   = N { _nnumber :: Integer
                      ,_labels :: Set VertexLabel
                     }deriving (Show)

instance Eq Neighbour where
   (N{_nnumber=n1}) == (N{_nnumber=n2}) = n1 == n2

instance Ord Neighbour where
   compare (N{_nnumber=n1}) (N{_nnumber=n2}) = compare n1 n2

L.makeLenses ''Neighbour

--Data Graph = Seq Vertex
data Vertex = V
     { _value  :: Word8  -- value
      ,_vnumber :: Integer  -- generation counter. Vertexes are created sequentially and this is number in sequence of creation
      ,_metric :: Word
      ,_neighbours :: Set Neighbour
     } deriving (Show,Eq)

instance Ord Vertex where
   compare (V{_vnumber=v1}) (V{_vnumber=v2}) = compare v1 v2


L.makeLenses ''Vertex


data BudgetForActions =
   B { _calls :: Integer
      ,_memory :: Integer
     }deriving (Show)
L.makeLenses ''BudgetForActions


data Context =
   C { _graph :: AdjacencyMap Vertex
      ,_entrancesLow  :: Seq Vertex
      ,_entrancesHigh :: Seq Vertex
     -- ,_freeCells     :: Seq Int
     -- ,_maxCells      :: Word
      ,_budget :: BudgetForActions
      ,_tick   :: Tick
     }deriving (Show)
L.makeLenses ''Context







data ApplicationLog =
    RanOutOfCalls    String
   |RanOutOfMemory   String
   |FunctionComplete String
   |OtherError       String
   |Note             String
   deriving (Show)


data Input = I {
    _input :: Maybe Word8
   ,_think :: Bool
   ,_answer :: Bool
   }deriving (Show)
L.makeLenses ''Input

type A' m = ExceptT ApplicationLog (RWST BudgetForActions (DList ApplicationLog) Context m)
type A = A' Identity







readFile_tc :: String -> Source A Input
readFile_tc n = do
   --f <- liftIO $ readFile n
   sourceList $ map x $ map words $ lines n
   where
    x [i, t, a] = I { _input = Just $ read i
                     ,_think = read t
                     ,_answer = read a
                    }
    x _         = I { _input = Nothing
                     ,_think = False
                     ,_answer = False
                    }


sink' :: Sink Word8 A [Word8]
sink' = consume

sink :: Sink (Word8, Context) A [(Word8, Context)]
sink = consume

step :: Conduit Input A (Word8, Context)
step = do
   ar <- await
   case ar of
    Nothing -> return ()
    Just a -> do

      yieldM (do
         r<-maybe (return 0) spc_basic $ a^.input
         c<-get
         return (r,c)
         )
      --yieldM (maybe (return 0) spc_basic $ a^.input)
      step



lastIndex x = (Sq.length x) -1

{-
connectToTrailing :: Maybe Vertex
   ->  -> A (AdjacencyMap Vertex)
connectToTrailing gn = do
   c<-get
-}

vertexEmpty = V  { _value   = 0
                  ,_vnumber = 0
                  ,_metric  = 0
                  ,_neighbours = S.empty
                 }

neighbourEmpty = N { _nnumber = 0
                    ,_labels  = S.empty
                   }

addNeighbour :: Vertex -> [VertexLabel] -> Vertex -> Vertex
addNeighbour v l n =
   set
      neighbours
      (insert (vertexToNeighbour n l) $ v^.neighbours)
      v

addNeighbourN :: Vertex -> Neighbour -> Vertex
addNeighbourN v n =
   set
      neighbours
      (insert n $ v^.neighbours)
      v

addNeighbours :: Vertex        --add neighbours to this vertex
              -> [VertexLabel] --all neighbours will get the same set of labels
              -> [Vertex]      --vertexses that are supposed to be added as neighbours
              -> Vertex
addNeighbours v l n = foldl (\x-> addNeighbour x l) v n

addNeighboursN :: Vertex -> [Neighbour] -> Vertex
addNeighboursN v [] = v
addNeighboursN v n = foldl addNeighbourN v n

vertexToNeighbour :: Vertex -> [VertexLabel] -> Neighbour
vertexToNeighbour v l = set_labels $ set_nnumber neighbourEmpty
   where
   set_labels  = set labels $ S.fromList l
   set_nnumber = set nnumber $ v^.vnumber


{-
addNeighboursFromAdjacencyList :: Vertex -> [(Vertex, ([Vertex],[VertexLabel]))] -> Vertex
addNeighboursFromAdjacencyList
-}

getTick :: A Integer
getTick = do
   c<-get
   assign tick $ Tick (1 + (u $ c^.tick))
   return $ u $ c^.tick
   where
      u :: Tick -> Integer
      u (Tick x) = x
      u _ = 0


--closestByTick :: Vertex -> [Vertex] -> Vertex


closestByTickAfter :: Vertex -> Set Vertex -> Maybe Vertex
closestByTickAfter v vs = lookupGT v vs

closestByTickAfterL :: Vertex -> [Vertex] -> Maybe Vertex
closestByTickAfterL v vs = closestByTickAfterL_common l
   where
      l = dropWhile (<=v) vs

closestByTickAfterL_common :: Eq a => [a] -> Maybe a
closestByTickAfterL_common l
   |l==[] = Nothing
   |otherwise = Just $ head l



closestByTickAfterVL :: Vertex -> [(Vertex, VertexLabel)] -> Maybe (Vertex, VertexLabel)
closestByTickAfterVL v vs = closestByTickAfterL_common l
   where
      l = dropWhile (\x-> (fst x) <=v) vs

lastLowEntrance :: A (Maybe Vertex)
lastLowEntrance = do
   c<-get
   return $ Sq.lookup (lastIndex (c^.entrancesLow)) (c^.entrancesLow)


getByLabel_common :: Set Neighbour -> Vertex -> A (Set Vertex)
getByLabel_common labelNs v = do
   c<-get
   return $ labelV v $ c^.graph
   where
      labelV  v g = S.filter (hasLabel v) $ postSet v g
         where
         hasLabel  v x = member (x^.vnumber) $ keepNnumbersOnly $ labelNs


getByLabel :: [VertexLabel] -> Vertex -> A (Set Vertex)
getByLabel l v = getByLabel_common (labelNs v) v
   where
   labelNs v = S.filter (\x-> not $ null $ intersection (S.fromList l) $ x^.labels) $ v^.neighbours



getByExactLabelOpen :: [VertexLabel] -> Vertex -> A (Set Vertex)
getByExactLabelOpen l v = getByLabel_common (labelNs v) v
   where
   labelNs v = S.filter (\x-> and $ map (\ll-> member ll $ x^.labels) l) $ v^.neighbours


getByLabelExclusiveExact :: [VertexLabel] -> Vertex -> A (Set Vertex)
getByLabelExclusiveExact l v = getByLabel_common (labelNs v) v
   where
   labelNs v = S.filter (\x-> and $ map (\ll-> notMember ll $ x^.labels) l) $ v^.neighbours

getByLabelExclusive :: [VertexLabel] -> Vertex -> A (Set Vertex)
getByLabelExclusive l v = getByLabel_common (labelNs v) v
   where
   labelNs v = S.filter (\x-> or $ map (\ll-> notMember ll $ x^.labels) l) $ v^.neighbours


getByExactLabel :: [VertexLabel] -> Vertex -> A (Set Vertex)
getByExactLabel l v = getByLabel_common (labelNs v) v
   where
   labelNs v = S.filter (\x-> (S.fromList l) == x^.labels) $ v^.neighbours




keepNnumbersOnly = S.map (\x-> x^.nnumber)
keepNnumbersOnlyL =  map (\x-> x^.nnumber)


correctionToPrediction :: Vertex --input
                       -> Vertex --Prediction about input
                       -> A (Vertex, VertexLabel) --Correction
correctionToPrediction i p = do
   nct <- getTick
   let nc = set value (abs err) $ set vnumber nct vertexEmpty
   case (err<0) of
      False -> return (nc, CPositive)
      True  -> return (nc, CNegative)

   where
      err = i^.value - p^.value



adjustPrediction :: Vertex --Prediction about input
                  -> (Vertex, VertexLabel) --Correction
                  -> Vertex --adjusted prediction
adjustPrediction p (c, CPositive) = set value (p^.value + c^.value) p
adjustPrediction p (c, CNegative) = set value (p^.value - c^.value) p
adjustPrediction p (c, _)         = p



getPredictions :: Vertex -> A (Set Vertex)
getPredictions v = getByLabel [Prediction] v

getPreviousPredictions :: Vertex -> A (Set Vertex)
getPreviousPredictions v = getByExactLabelOpen [Prediction,Backward] v

getCorrections :: Vertex -> A (Set Vertex)
getCorrections v = getByLabel [CPositive,CNegative] v

{-
attachPreviousPredictionsToNewInput :: Vertex   --New input
                                    -> [Vertex] --PreviousPredictions
                                    ->
-}

{-- =================================================
Save Predict Correct - basic version
--}
spc_basic :: Word8 -> A Word8
spc_basic i = do
   c<-get
   ni <- getTick
   np <- getTick
   let niv  = set value i $ set vnumber ni vertexEmpty --new input vertex
   let nbpv = set value i $ set vnumber np vertexEmpty --basic prediction
   le <- lastLowEntrance

   case le of
      Nothing -> do  --first value entered. The beginning entry.
         let niv'  = addNeighbour niv [Prediction] nbpv
         let nbpv' = addNeighbour nbpv [Input] niv

         assign graph
            $ overlay (c^.graph) (fromAdjacencyList [(niv' , [nbpv'])
                                          , (nbpv', [niv'])])
         assign entrancesLow $ c^.entrancesLow |> niv'
         return $ nbpv'^.value

      Just v  -> do --not first entry. General computation.

         pps<- (return.elems) =<< getPredictions v
         pcs <- mapM (\x->do
            x'<-getPreviousPredictions x
            y <-mapM (\x-> do
                        x' <- getCorrections x
                        return $ elems x'
                        ) $ elems x'
            return $ concat y
            ) pps


         ctps <- mapM (correctionToPrediction niv) pps

         let addNeighbour_flipArgs  l n v = addNeighbour  v l n
         let addNeighbours_flipArgs l n v = addNeighbours v l n

         let niv'  = addNeighbour_flipArgs [Same,Backward] v
                        $ addNeighbour niv [Prediction] nbpv



         let nbpv' = (\x-> maybe x (adjustPrediction x) $ closestByTickAfterVL v ctps
                         )
                         $ addNeighbours_flipArgs [Same,Backward] pps
                         $ addNeighbour nbpv [Input] niv

         let v' = addNeighbour v [Same,Forward] niv
         let pps' = map (\(p, (c,l))->
                           addNeighbour_flipArgs [l] c
                              $ addNeighbour_flipArgs [Same,Forward] nbpv' p
                        )
                           $ zip pps ctps

         let pcs' = concatMap (\(pc,c)->
                                 zip
                                     (map (addNeighbour_flipArgs [Same,Forward] (fst c)
                                          ) pc
                                     )
                                     $ repeat $ fst c
                              )
                              $ zip pcs ctps

         let cs' = map (\(c,pc)-> (addNeighbours (fst c) [Same,Backward] pc, pc)
                       ) $ zip ctps pcs

         assign graph
            $ overlay (c^.graph) (fromAdjacencyList $ concat
               [
               [(niv' , [nbpv', v])
               ,(nbpv', niv':pps)
               ,(v'   , [niv'])
               ]

               ,(zip pps'
                    $ map (\(l,r)-> [l,r])
                        $ zip (map fst ctps)
                              $ repeat nbpv'
                )
               ,(map (\(pc, c)->(pc, [c])) pcs'
                )
               ,cs'
               ])

         assign entrancesLow $ c^.entrancesLow |> niv'
         return $ nbpv'^.value







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
