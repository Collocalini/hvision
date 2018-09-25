
--{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Type_22052017_classifier (
 step
,sink
,sink''
,output
,naiveResult
,readFile_tc
,graphvisCompatible
,Vertex(..)
,Input(..)
,A
,Context(..)
,Tick(..)
,BudgetForActions(..)
) where


import Data.Sequence hiding (drop,map,intersperse,length,empty,fromList,null,zip,filter,lookup)
import qualified Data.Sequence as Sq
import Data.Set hiding (drop,insert,map,filter,empty,fromList,null,foldl,foldl')
import qualified Data.Set as S
import Algebra.Graph.AdjacencyMap hiding (empty,graph)
import qualified Algebra.Graph.AdjacencyMap as Ag
import Algebra.Graph.Export.Dot -- hiding ()
--import qualified Algebra.Graph.Export.Dot as Agxd
import Control.Lens hiding ((.=),Context,(|>))
import qualified Control.Lens as L --hiding (element)
import Data.DList hiding (tail,map,empty,fromList,head,concat,lookup)
import qualified Data.DList as DL
import Data.List
import Data.Maybe
import Data.Word
import Control.Monad.RWS.Lazy
import Control.Monad.Except
import Control.Monad.Extra
import Data.Conduit
import Data.Conduit.List hiding (drop,map,mapM,head,concatMap,concat,filter,lookup,concatMapM)
import qualified Data.Conduit.List as Cl
--import Data.Lens.Template




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


data Return_of_spc_arbitraryLE = Return_of_spc_arbitraryLE {
    _spc_arbitraryLE_output :: Word8
   ,_spc_arbitraryLE_niv    :: Vertex
   } deriving (Show)
L.makeLenses ''Return_of_spc_arbitraryLE




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
      ,_verbosity ::[String]
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


verboseTell :: String -> String -> A ()
verboseTell v s = do
  c<-get
  case (elem v $ c^.verbosity)of
    False -> return ()
    True  -> tell $ DL.singleton $ Note s




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



output :: String -> (Word8, String) -> IO ()
output f (o, g) = do
   putStrLn $ show o
   writeFile (f ++ ".dot") g

naiveResult (Identity ((Right x), log)) = (x,log)


sink' :: Sink Word8 A [Word8]
sink' = consume

sink :: Sink (Word8, Context) A [(Word8, Context)]
sink = consume

sink'' :: Sink (Word8, String) A [(Word8, String)]
sink'' = consume

step :: Conduit Input A (Word8, Context)
step = do
   ar <- await
   case ar of
    Nothing -> return ()
    Just a -> do

      yieldM (do
         r<-maybe (return 0) spc_fecalePDN $ a^.input
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
      (S.insert (vertexToNeighbour n l) $ v^.neighbours)
      v

addNeighbourN :: Vertex -> Neighbour -> Vertex
addNeighbourN v n =
   set
      neighbours
      (S.insert n $ v^.neighbours)
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

closestByTickBeforeL :: Vertex -> [Vertex] -> Maybe Vertex
closestByTickBeforeL v vs = listToMaybe l
   where
      l = dropWhile (>=v) vs


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
      labelV  v g = S.filter (hasLabel v) $ vertexSet g -- $ postSet v g
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
   let nc = set value (i^.value - p^.value) $ set vnumber nct vertexEmpty
   return (nc, CPositive)
   {-let nc = set value absErr $ set vnumber nct vertexEmpty
   case (err<0) of
      False -> return (nc, CPositive)
      True  -> return (nc, CNegative)

   where
      err :: Int 
      err = iv - pv
      absErr = fromIntegral $ abs err
      
      iv  = fromIntegral $ i^.value
      pv  = fromIntegral $ p^.value
-}


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

getSamePrevious :: Vertex -> A (Set Vertex)
getSamePrevious v = getByExactLabelOpen [Same,Backward] v

getCorrections :: Vertex -> A (Set Vertex)
getCorrections v = getByLabel [CPositive,CNegative] v

{-
attachPreviousPredictionsToNewInput :: Vertex   --New input
                                    -> [Vertex] --PreviousPredictions
                                    ->
-}




defaultReturn_of_spc_arbitraryLE = Return_of_spc_arbitraryLE {
    _spc_arbitraryLE_output = 0
   ,_spc_arbitraryLE_niv    = vertexEmpty
   } 

{-- =================================================
Save Predict Correct - basic version
--}
spc_basic :: Word8 -> A Word8
spc_basic i = do
   c<-get
   ni <- getTick
   np <- getTick
   let niv  = setVertexValueAndVNumber i ni vertexEmpty --new input vertex
   let nbpv = setVertexValueAndVNumber i np vertexEmpty --basic prediction
   le <- lastLowEntrance
   
   rospcale<- spc_arbitraryLE niv nbpv le i
   
   c<-get
   assign entrancesLow $ c^.entrancesLow |> (rospcale^.spc_arbitraryLE_niv)
   return $ rospcale^.spc_arbitraryLE_output
   
   where
      setVertexValueAndVNumber i ni v = set value i $ set vnumber ni v


{-- =================================================
Save Predict Correct - feed correction to spc_arbitraryLE
--}
spc_feed_correction :: Word8 -> A Word8
spc_feed_correction i = do
   c<-get
   ni <- getTick
   np <- getTick
   let niv  = setVertexValueAndVNumber i ni vertexEmpty --new input vertex
   let nbpv = setVertexValueAndVNumber i np vertexEmpty --basic prediction
   le <- lastLowEntrance
   
   rospcale<- spc_arbitraryLE niv nbpv le i
   
   c<-get
   assign entrancesLow $ c^.entrancesLow |> (rospcale^.spc_arbitraryLE_niv)
   c<-get
   
   -- .................
   
   --pps<- le2pps le
   pcs<- pps2pcs =<< le2pps le
   
   verboseTell "spc_feed_correction" ("spc_feed_correction pcs= " ++ (show pcs))
   
   ifSingle_PCS_then_spc_arbitraryLE pcs
   -- .................
   
   return $ rospcale^.spc_arbitraryLE_output
   
   where
      setVertexValueAndVNumber i ni v = set value i $ set vnumber ni v
      
      le2pps (Just le) = (return.elems) =<< getPredictions le
      le2pps Nothing   = return []
      
      pps2pcs pps = previousCorrectionS pps
      
      previousCorrectionS :: [Vertex] -> A [Vertex]
      previousCorrectionS pps = concatMapM (pps2pcs') pps
         where 
            pps2pcs' :: Vertex -> A [Vertex]
            pps2pcs' pps = do
               pcs <- getCorrections pps
               return $ elems pcs
               
      
      ifSingle_PCS_then_spc_arbitraryLE [niv] = do 
         c<-get
         np <- getTick
         let nbpv = setVertexValueAndVNumber (niv^.value) np vertexEmpty
         le <- (return.listToMaybe.elems) =<< getSamePrevious niv
         rospcale<- spc_arbitraryLE niv nbpv le (niv^.value)
         return $ Just $ rospcale
         
         
      ifSingle_PCS_then_spc_arbitraryLE _ = return $ Nothing



{-- =================================================
Save Predict Correct - Feed Every Correction to spc_arbitraryLE As Low Entrance
--}
spc_fecale :: Word8 -> A Word8
spc_fecale i = do
   c<-get
   ni <- getTick
   np <- getTick
   let niv  = setVertexValueAndVNumber i ni vertexEmpty --new input vertex
   let nbpv = setVertexValueAndVNumber i np vertexEmpty --basic prediction
   le <- lastLowEntrance
   
   rospcale<- spc_arbitraryLE niv nbpv le i
   
   c<-get
   assign entrancesLow $ c^.entrancesLow |> (rospcale^.spc_arbitraryLE_niv)
   c<-get
   
   -- .................
   
   run_exhaustive_spc_arbitraryLE_over_corrections $ Just rospcale
   
   -- .................
   
   return $ rospcale^.spc_arbitraryLE_output
   
   where
      setVertexValueAndVNumber i ni v = set value i $ set vnumber ni v
      
      le2ple (Just le) = (return.nothingIfnotSingle.elems) =<< getSamePrevious le
         where 
            nothingIfnotSingle [x] = Just x
            nothingIfnotSingle _   = Nothing

      le2ple Nothing   = return Nothing
      
      le2pps (Just le) = (return.elems) =<< getPredictions le
      le2pps Nothing   = return []
      
      pps2pcs pps = previousCorrectionS pps
      
      previousCorrectionS :: [Vertex] -> A [Vertex]
      previousCorrectionS pps = concatMapM (pps2pcs') pps
         where 
            pps2pcs' :: Vertex -> A [Vertex]
            pps2pcs' pps = do
               pcs <- getCorrections pps
               return $ elems pcs
               
      
      ifSingle_PCS_then_spc_arbitraryLE [niv] = do 
         c<-get
         np <- getTick
         let nbpv = setVertexValueAndVNumber (niv^.value) np vertexEmpty
         le <- (return.listToMaybe.elems) =<< getSamePrevious niv
         rospcale<- spc_arbitraryLE niv nbpv le (niv^.value)
         return $ Just $ rospcale
         
         
      ifSingle_PCS_then_spc_arbitraryLE _ = return $ Nothing
      
      
      pcs_pps_ple_le (Just r) = pps2pcs =<< le2pps =<< le2ple (Just $ r^.spc_arbitraryLE_niv)
      pcs_pps_ple_le _        = return []


      run_exhaustive_spc_arbitraryLE_over_corrections r = do 
         pcs<- pcs_pps_ple_le r
         verboseTell "spc_fecale" ("spc_fecale pcs1= " ++ (show pcs))
         continueIfHas_pcs pcs
         
         where 
            continueIfHas_pcs [] = ifSingle_PCS_then_spc_arbitraryLE []
            continueIfHas_pcs pcs = 
               run_exhaustive_spc_arbitraryLE_over_corrections 
                  =<< ifSingle_PCS_then_spc_arbitraryLE pcs





{-- =================================================
Save Predict Correct - Feed Every Correction to spc_arbitraryLEpdn As Low Entrance
--}
spc_fecalePDN :: Word8 -> A Word8
spc_fecalePDN i = do
   c<-get
   ni <- getTick
   np <- getTick
   let niv  = setVertexValueAndVNumber i ni vertexEmpty --new input vertex
   let nbpv = setVertexValueAndVNumber i np vertexEmpty --basic prediction
   le <- lastLowEntrance
   
   rospcale<- spc_arbitraryLEpdn niv nbpv le i
   
   c<-get
   assign entrancesLow $ c^.entrancesLow |> ((fromJust rospcale)^.spc_arbitraryLE_niv)
   c<-get
   
   -- .................
   
   run_exhaustive_spc_arbitraryLEpdn_over_corrections rospcale
   
   -- .................
   
   return $ (fromJust rospcale)^.spc_arbitraryLE_output
   
   where
      setVertexValueAndVNumber i ni v = set value i $ set vnumber ni v
      
      le2ple (Just le) = (return.nothingIfnotSingle.elems) =<< getSamePrevious le
         where 
            nothingIfnotSingle [x] = Just x
            nothingIfnotSingle _   = Nothing

      le2ple Nothing   = return Nothing
      
      le2pps (Just le) = (return.elems) =<< getPredictions le
      le2pps Nothing   = return []
      
      pps2pcs pps = previousCorrectionS pps
      
      previousCorrectionS :: [Vertex] -> A [Vertex]
      previousCorrectionS pps = concatMapM (pps2pcs') pps
         where 
            pps2pcs' :: Vertex -> A [Vertex]
            pps2pcs' pps = do
               pcs <- getCorrections pps
               return $ elems pcs
               
      
      ifSingle_PCS_then_spc_arbitraryLEpdn [niv] = do 
         c<-get
         np <- getTick
         let nbpv = setVertexValueAndVNumber (niv^.value) np vertexEmpty
         le <- (return.listToMaybe.elems) =<< getSamePrevious niv
         rospcale<- spc_arbitraryLEpdn niv nbpv le (niv^.value)
         return rospcale
         
         
      ifSingle_PCS_then_spc_arbitraryLEpdn _ = return $ Nothing
      
      
      pcs_pps_ple_le (Just r) = pps2pcs =<< le2pps =<< le2ple (Just $ r^.spc_arbitraryLE_niv)
      pcs_pps_ple_le _        = return []


      run_exhaustive_spc_arbitraryLEpdn_over_corrections r = do 
         pcs<- pcs_pps_ple_le r
         verboseTell "spc_fecalePDN" ("spc_fecalePDN pcs1= " ++ (show pcs))
         continueIfHas_pcs pcs
         
         where 
            continueIfHas_pcs [] = ifSingle_PCS_then_spc_arbitraryLEpdn []
            continueIfHas_pcs pcs = 
               run_exhaustive_spc_arbitraryLEpdn_over_corrections 
                  =<< ifSingle_PCS_then_spc_arbitraryLEpdn pcs








{-- =================================================
Save Predict Correct - arbitrary last low entrance version
--}
spc_arbitraryLE   :: Vertex 
                  -> Vertex 
                  -> Maybe Vertex 
                  -> Word8 
                  -> A Return_of_spc_arbitraryLE
spc_arbitraryLE niv nbpv le i = do
   c<-get
   case le of
      Nothing -> do  --first value entered. The beginning entry.
         let niv'  = addNeighbour niv [Prediction] nbpv
         let nbpv' = addNeighbour nbpv [Input] niv

         updateVertexes (c^.graph) [(niv,niv')]
         c<-get

         assign graph
            $ overlay (c^.graph) (fromAdjacencyList [(niv' , [nbpv'])
                                          , (nbpv', [niv'])])
         --assign entrancesLow $ c^.entrancesLow |> niv'
         --return $ nbpv'^.value
         return 
            $ set spc_arbitraryLE_niv niv' 
            $ set spc_arbitraryLE_output (nbpv'^.value) defaultReturn_of_spc_arbitraryLE

      Just v  -> do --not first entry. General computation.
         verboseTell "spc" ("spc_arbitraryLE")
         
         pps<- previousPredictionS v
         ctps <- newCorrections niv pps

         let niv'  = niv_AddNeighbours niv nbpv v
         let nbpv' = nbpv_adjustPrediction v ctps $ nbpv_AddNeighbours nbpv niv pps
         let v'    = addNeighbour v [Same,Forward] niv
         let pps'  = pps_AddNeighbours ctps pps nbpv'
         

         verboseTell "spc" ("pps'=" ++ (show pps'))
         verboseTell "spc" ("ctps=" ++ (show ctps))
         
         updateVertexes (c^.graph) [(niv,niv')]
         c<-get
         
         updateVertexes (c^.graph) $ zip pps pps'
         c<-get
         updateVertexes (c^.graph)  [(v, v')]
           
         c<-get
         assign graph
            $ overlay (c^.graph) (fromAdjacencyList $ concat
               [
                  [(niv' , [nbpv', v'])
                  ,(nbpv', niv':pps')
                  ,(v'   , [niv'])
                  ]

                  ,(zip pps'
                       $ pps_edgesTo_ctps_and_nbpv ctps nbpv'
                   )

               ])
         
         c<-get

         pcs      <- previousCorrectionS pps'
         let pcs'  = pcs_AddNeighbours ctps pcs 
         let cs    = cs_AddNeighbours ctps pcs' pps'

         verboseTell "spc" ("pcs=" ++ (show pcs))
         verboseTell "spc" ("pcs'=" ++ (show pcs'))
         verboseTell "spc" ("cs=" ++ (show cs))

         updateVertexes (c^.graph) $ zip pcs pcs'
         c<-get
         updateVertexes (c^.graph) $ zip (map fst ctps) cs

         c<-get

         assign graph
            $ overlay (c^.graph) (fromAdjacencyList $ concat
               [
                   zip pcs' $ repeat $ map fst ctps
                  ,zip cs   $ repeat pcs'
                  ,zip cs   $ repeat pps'
               ])

         
         c<-get
         verboseTell "spc" ("graph=" ++ (show $ c^.graph))
         
         --assign entrancesLow $ c^.entrancesLow |> niv'
         --return $ nbpv'^.value
         return 
            $ set spc_arbitraryLE_niv niv' 
            $ set spc_arbitraryLE_output (nbpv'^.value) defaultReturn_of_spc_arbitraryLE
   where
    updateVertexes :: AdjacencyMap Vertex -> [(Vertex,Vertex)] -> A ()
    updateVertexes oldGraph zip_Vold_Vnew = do 
      assign graph $ 
            foldl' (\g (l,r)-> replaceVertex l r g) oldGraph zip_Vold_Vnew
    
    

    previousPredictionS v = (return.elems) =<< getPredictions v
    
    previousCorrectionS :: [Vertex] -> A [Vertex]
    previousCorrectionS pps = concatMapM (pps2ppps2pcs) pps
      where 
         pps2ppps2pcs :: Vertex -> A [Vertex]
         pps2ppps2pcs pps = do
            ppps<- getSamePrevious pps
            pcs <-mapM ppps2pcs $ elems ppps
            --pcs <- mapM ppps2pcs [pps] 
            return $ concat pcs
            
            where
               ppps2pcs :: Vertex -> A [Vertex]
               ppps2pcs ppps = do
                  verboseTell "spc" ("ppps2pcs ppps=" ++ (show ppps))
                  pcs <- getCorrections ppps
                  verboseTell "spc" ("ppps2pcs pcs=" ++ (show pcs))
                  return $ elems pcs

    newCorrections niv pps = mapM (correctionToPrediction niv) pps

    addNeighbour_flipArgs  l n v = addNeighbour  v l n
    addNeighbours_flipArgs l n v = addNeighbours v l n

    niv_AddNeighbours niv nbpv v = addNeighbour_flipArgs [Same,Backward] v
                                              $ addNeighbour niv [Prediction] nbpv

    nbpv_AddNeighbours nbpv niv pps = addNeighbours_flipArgs [Same,Backward] pps
                                        $ addNeighbour nbpv [Input] niv

    nbpv_adjustPrediction v ctps nbpv = maybe nbpv (adjustPrediction nbpv)
                                              $ closestByTickAfterVL v ctps

    pps_AddNeighbours ctps pps nbpv' =
      map (\(p, (c,l))->
                        addNeighbour_flipArgs [l] c
                            $ addNeighbour_flipArgs [Same,Forward] nbpv' p
          )
             $ zip pps ctps

    pcs_AddNeighbours ctps pcs =
      concatMap addSameForward pcs
      where
         addSameForward pc = map (\c->addNeighbour_flipArgs [Same,Forward] c pc)
                                 $ map fst ctps


    cs_AddNeighbours ctps pcs pps =
      map (addInput.addSameBackward.fst) ctps
      where
         addSameBackward c = addNeighbours c [Same,Backward] pcs
         addInput        c = addNeighbours c [Input]         pps
         

    vertexesFrom_ctps ctps = map fst ctps
    
    pps_edgesTo_ctps_and_nbpv ctps nbpv' = repeat $ nbpv':(vertexesFrom_ctps ctps)
 









{-- =================================================
Save Predict Correct - arbitrary last low entrance, save predict and correct only if predict didn't
match. 
--}
spc_arbitraryLEpdn   :: Vertex 
                  -> Vertex 
                  -> Maybe Vertex 
                  -> Word8 
                  -> A (Maybe Return_of_spc_arbitraryLE)
spc_arbitraryLEpdn niv nbpv le i = do
   c<-get
   case le of
      Nothing -> do  --first value entered. The beginning entry.
         let niv'  = addNeighbour niv [Prediction] nbpv
         let nbpv' = addNeighbour nbpv [Input] niv

         updateVertexes (c^.graph) [(niv,niv')]
         c<-get

         assign_graph_overlayC [(niv' , [nbpv'])
                              , (nbpv', [niv'])]
                              
         --assign entrancesLow $ c^.entrancesLow |> niv'
         --return $ nbpv'^.value
         return $ Just
            $ set spc_arbitraryLE_niv niv' 
            $ set spc_arbitraryLE_output (nbpv'^.value) defaultReturn_of_spc_arbitraryLE

      Just v  -> do --not first entry. General computation.
         verboseTell "spc" ("spc_arbitraryLEpdn")
         
         pps<- previousPredictionS v
         ctps <- newCorrections niv pps

         continueIfppsDidntMatchNiv niv pps nbpv ctps v

         
   where
    updateVertexes :: AdjacencyMap Vertex -> [(Vertex,Vertex)] -> A () -- update graph from argument
    updateVertexes oldGraph zip_Vold_Vnew = do 
      assign graph $ 
            foldl' (\g (l,r)-> replaceVertex l r g) oldGraph zip_Vold_Vnew
    
    updateVertexesC :: [(Vertex,Vertex)] -> A () -- update graph from context
    updateVertexesC zip_Vold_Vnew = do 
      c<-get
      let oldGraph = c^.graph
      updateVertexes oldGraph zip_Vold_Vnew
    
    assign_graph_overlayC :: [(Vertex, [Vertex])] -> A ()
    assign_graph_overlayC al = do 
      c<-get
      assign graph $ overlay (c^.graph) (fromAdjacencyList al)

    previousPredictionS le = (return.elems) =<< getPredictions le
    
    previousCorrection :: Vertex -> A (Maybe Vertex)
    previousCorrection pp = do 
      ppp<- goBack pp
      verboseTell "spc" ("previousCorrection pp=" ++ (show pp))
      verboseTell "spc" ("previousCorrection ppp=" ++ (show ppp))
      ppp2pc ppp
      
      where 
         goBack :: Vertex -> A (Maybe Vertex)
         goBack v = do 
            pv <- getSamePrevious v
            return $ listToMaybe $ elems pv
 
         ppp2pc :: Maybe Vertex -> A (Maybe Vertex)
         ppp2pc (Just ppp) = do
            verboseTell "spc" ("ppp2pc ppp=" ++ (show ppp))
            pc <- getCorrections ppp
            verboseTell "spc" ("ppp2pc pc=" ++ (show pc))
            ifHasNoPC_run_pp2pc $ elems pc
            
            where 
               ifHasNoPC_run_pp2pc [] = previousCorrection ppp
               ifHasNoPC_run_pp2pc x  = return $ closestByTickAfterL ppp x
         
         ppp2pc Nothing = return $ Nothing

    newCorrections niv pps = mapM (correctionToPrediction niv) pps

    addNeighbour_flipArgs  l n v = addNeighbour  v l n
    addNeighbours_flipArgs l n v = addNeighbours v l n

    niv_AddNeighbours niv nbpv le = addNeighbour_flipArgs [Same,Backward] le
                                              $ addNeighbour niv [Prediction] nbpv

    nbpv_AddNeighbours nbpv niv pp = addNeighbour_flipArgs [Same,Backward] pp
                                        $ addNeighbour nbpv [Input] niv

    nbpv_adjustPrediction ctp nbpv = adjustPrediction nbpv ctp


    pp_AddNeighbours ctp pp nbpv' = addNeighbour_flipArgs [(snd ctp)] (fst ctp)
                                       $ addNeighbour_flipArgs [Same,Forward] nbpv' pp
                                       
    pp_AddNeighboursNoCtp pp nbpv' = addNeighbour_flipArgs [Same,Forward] nbpv' pp
          

    pc_AddNeighbours pc ctp = addNeighbour pc [Same,Forward] (fst ctp)


    cs_AddNeighbours ctp pc pp =
      (addInput.addSameBackward.fst) ctp
      where
         addSameBackward c = addNeighbour c [Same,Backward] pc
         addInput        c = addNeighbour c [Input]         pp
         


    
    
    continueIfppsDidntMatchNiv :: Vertex 
                                 -> [Vertex] 
                                 -> Vertex
                                 -> [(Vertex, VertexLabel)] 
                                 -> Vertex
                                 -> A (Maybe Return_of_spc_arbitraryLE)
    continueIfppsDidntMatchNiv niv [pp] nbpv [ctp] le
      |ctpIsNot0 = run 
      |otherwise = runCaseMatched
      where
         ctpIsNot0 = (fst ctp)^.value /= 0 
         
         run = do 

            let niv'  = niv_AddNeighbours niv nbpv le
            let nbpv' = nbpv_adjustPrediction ctp $ nbpv_AddNeighbours nbpv niv pp
            let le'   = addNeighbour le [Same,Forward] niv
            let pp'   = pp_AddNeighbours ctp pp nbpv'
            

            verboseTell "spc" ("pp'=" ++ (show pp'))
            verboseTell "spc" ("ctp=" ++ (show ctp))
            
            updateVertexesC [(niv,niv')
                           ,(pp, pp')
                           ,(le, le')]

            
            assign_graph_overlayC
                  [
                      (niv' , [nbpv', le'])
                     ,(nbpv', [niv' , pp'])
                     ,(le'  , [niv'])

                     ,(pp' , [(fst ctp), nbpv'])

                  ]
 
            pc      <- previousCorrection pp'
            continueIfHas_pc pc pp' ctp niv' nbpv'
         
         
         continueIfHas_pc :: Maybe Vertex 
                              -> Vertex 
                              -> (Vertex, VertexLabel) 
                              -> Vertex 
                              -> Vertex 
                              -> A (Maybe Return_of_spc_arbitraryLE)
         continueIfHas_pc (Just pc) pp' ctp niv' nbpv' = do 

            let pc'  = pc_AddNeighbours pc ctp
            let cs   = cs_AddNeighbours ctp pc' pp'
            
            verboseTell "spc" ("continueIfHas_pc")
            verboseTell "spc" ("pc=" ++ (show pc))
            verboseTell "spc" ("pc'=" ++ (show pc'))
            verboseTell "spc" ("cs=" ++ (show cs))

            updateVertexesC [(pc, pc')
                            ,(fst ctp,cs)
                            ]



            assign_graph_overlayC
                  [
                      (pc' , [fst ctp])
                     ,(cs  , [pc'])
                     ,(cs  , [pp'])
                  ]

            
            c<-get
            verboseTell "spc" ("graph=" ++ (show $ c^.graph))
            
            return $ Just
               $ set spc_arbitraryLE_niv niv' 
               $ set spc_arbitraryLE_output (nbpv'^.value) defaultReturn_of_spc_arbitraryLE
               
         continueIfHas_pc Nothing pp' ctp niv' nbpv' = do 
            let cs   = addNeighbour (fst ctp) [Input] pp'
            
            verboseTell "spc" ("continueIfHas_pc no pc")
            verboseTell "spc" ("cs=" ++ (show cs))
            
            updateVertexesC [(fst ctp,cs)]

            assign_graph_overlayC
                  [
                  (cs  , [pp'])
                  ]

            
            c<-get
            verboseTell "spc" ("graph=" ++ (show $ c^.graph))
            
            return $ Just
               $ set spc_arbitraryLE_niv niv' 
               $ set spc_arbitraryLE_output (nbpv'^.value) defaultReturn_of_spc_arbitraryLE
         
         
         runCaseMatched = do 

            let niv'  = niv_AddNeighbours niv nbpv le
            let nbpv' = nbpv_AddNeighbours nbpv niv pp
            let le'   = addNeighbour le [Same,Forward] niv
            let pp'   = pp_AddNeighboursNoCtp pp nbpv' 
            

            verboseTell "spc" ("pp'=" ++ (show pp'))
            verboseTell "spc" ("ctp=" ++ (show ctp))
            
            updateVertexesC [(niv,niv')
                           ,(pp, pp')
                           ,(le, le')]
              

            assign_graph_overlayC
                  [
                      (niv' , [nbpv', le'])
                     ,(nbpv', [niv' , pp'])
                     ,(le'  , [niv'])
                     
                     ,(pp' , [nbpv'])

                  ]
            
            return $ Just
               $ set spc_arbitraryLE_niv niv' 
               $ set spc_arbitraryLE_output (nbpv'^.value) defaultReturn_of_spc_arbitraryLE

            
            

    continueIfppsDidntMatchNiv niv pps nbpv ctps le = do 
      tell $ DL.singleton 
         $ OtherError ("Didn't match pattern: continueIfppsDidntMatchNiv niv [pp] nbpv [ctp] le "
            ++ (show niv) ++ "; "
            ++ (show pps) ++ "; "
            ++ (show nbpv) ++ "; "
            ++ (show ctps) ++ "; "
            ++ (show le) 
            )
            
      return Nothing
      
    












graphvisCompatible :: Conduit (Word8, Context) A (Word8, String)
graphvisCompatible = do
   ar <- await
   case ar of
      Nothing -> return ()
      Just (w,c) -> do

         yield $ (w, export style $ c^.graph)

         graphvisCompatible



vertexToLable :: Vertex -> String
vertexToLable v = "#" ++ (show $ v^.vnumber)
                  ++ ", " ++ (show $ v^.value)
                  ++ " m=" ++ (show $ v^.metric)



edgeStyle :: Vertex -> Vertex -> [Attribute String]
edgeStyle a b = --["color" := "blue"   | odd ( a^.vnumber + b^.vnumber)]
  [colorStyle, edgeStyle]
  where
    prediction = caseEdgeLabel [Prediction]
    input      = caseEdgeLabel [Input]
    same       = caseEdgeLabel [Same]
    backward   = caseEdgeLabel [Backward]
    forward    = caseEdgeLabel [Forward]
    cPositive  = caseEdgeLabel [CPositive]
    cNegative  = caseEdgeLabel [CNegative]

    edgeAB n = n^.nnumber == b^.vnumber
    caseEdgeLabel l = or $ S.map (\n-> isSubsetOf (S.fromList l) $ n^.labels)
                                  $ S.filter edgeAB $ a^.neighbours


    attrComb = [
       (prediction , "blue")
      ,(input      , "brown")
      ,(same       , "grey")
      ,(backward   , "red")
      ,(forward    , "green")
      ,(cPositive  , "magenta")
      ,(cNegative  , "indigo")
      ]

    colorStyle
      |hasLabel= "color" := (concat $ intersperse (";" ++ (show lengthPerLabel) ++ ":")
                                                 $ map snd
                                                 $ detectedLabels
                            )
      |otherwise = "color" := ""

    edgeStyle
      |hasLabel= "style" := "bold"
      |otherwise = "style" := "dotted"

    hasLabel = not $ null $ detectedLabels
    detectedLabels = filter fst attrComb
    lengthPerLabel = 1/(fromIntegral $ length detectedLabels)




setStyleEdgeAttributes x s = (\s -> s {edgeAttributes=x}) s
setStyleVertexName     x s = (\s -> s {vertexName=x})     s


style = setStyleEdgeAttributes edgeStyle
  $ defaultStyle vertexToLable



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
