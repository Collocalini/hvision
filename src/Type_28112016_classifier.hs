module Type_28112016_classifier (

) where

import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fld
import Data.List
import Data.Either
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer.Lazy

takeWhileFits :: Int -> Seq a -> [Seq a]
takeWhileFits i s 
   |i>(Seq.length s) = (Seq.take i s):(takeWhileFits i (Seq.drop 1 s))
   |otherwise = Seq.take i s


idFirst :: Int
idFirst s = 0
idLast :: Seq a -> Int
idLast s = (Seq.length s) -1



Data CrossCorellationErrors = LengthOfArgumentsMissmatch

crossCorellation :: Ord a => Seq a -> Seq a -> Either String (Seq a)
crossCorellation mask input
   |(Seq.length mask) == (Seq.length input) = 
                                             Right $ map (\i->   (Seq.index input i) 
                                                               - (Seq.index mask  i)
                                                         )-----------------------
                                                         [idFirst .. idLast mask]
   |otherwise = Left LengthOfArgumentsMissmatch



crossCorellation_idsAtMins :: Num Ord a => Seq a -> Seq a -> Seq Int
crossCorellation_idsAtMins mask input = Seq.fromList 
   $ fst
   $ unzip 
   $ mark_extremums Min $ (\y-> zip [1..] y) 
   -- $ Seq.fromList 
   $ map toScalar 
   $ rights
   $ map (\cut-> crossCorellation mask cut) 
   $ takeWhileFits (length mask) input
   where
      toScalar s = p4
         where
         p1 x = (x - avg s) ^ 2
         p2   = sum $ map p1 s
         p3   = p2/(Seq.length s)
         p4   = sqrt p3

      
      avg s = (Fld.sum s)/(Seq.length s)
   
   



Data Tree a = 
   Node { value     :: Ether ApplicationLog (Seq.Seq a)
         ,subForest :: Ether ApplicationLog (Seq.Seq (Tree a))
        }


type CorrelationAtMinsIdsTree = Tree Int



Data Graph = Seq.Seq Vertex
Data Vertex = 
   V { 
      ,value :: Word8  -- value
      ,number :: Int   -- generation counter. Vertexes are created sequentially and this is number in sequence of creation
      ,usefulness :: Word
      ,neighbours :: Seq.Seq Neighbour
     }

Data Neighbour   = N { position :: Int
                      ,labels :: Seq.Seq VertexLabel
                     }
Data VertexLabel = BwaaMore|BwaaLess|BwaaSame




Data BudgetForActions = 
   B { calls :: Word
      ,memory :: Word
     }



type ContextStackT m a = StateT Context (ErrorT ApplicationLog m) a
type ContextStack    a = ContextStackT Identity a
type ContextStackVoid  = ContextStackT Identity ()

type ContextStackWithLogT m a = ContextStackT (WriterT [String] m) a
type ContextStackWithLog    a = ContextStackWithLogT Identity a
type ContextStackWithLogVoid  = ContextStackWithLogT Identity ()

type BudgetAwareFuncT m a = StateT BudgetForActions (ErrorT ApplicationLog m) a
type BudgetAwareFunc    a = BudgetAwareFuncT Identity a
type BudgetAwareFuncVoid  = BudgetAwareFuncT Identity ()


type BudgetAwareFuncWithLogT m a = BudgetAwareFuncT (WriterT [String] m) a
type BudgetAwareFuncWithLog    a = BudgetAwareFuncWithLogT Identity a
type BudgetAwareFuncWithLogVoid  = BudgetAwareFuncWithLogT Identity ()




Data Context = 
   C { graph :: Graph
      ,entrancesLow  :: Seq.Seq Int
      ,entrancesHigh :: Seq.Seq Int
      ,budget :: BudgetForActions
     }

Data ContextErrors = 
    RanOutOfCalls String
   |RanOutOfMemory String
   |OtherError String

instance Show ContextErrors where
  show (RanOutOfCalls s) = "Allowed number of calls exceeded" ++ s
  show (RanOutOfMemory s) = "Allowed amount of memory exceeded" ++ s
  show (OtherError str) = str


instance Error ContextErrors where
   noMsg    = OtherError "ContextError occurred!"
   strMsg s = OtherError s


Data ApplicationLog = 
    Include1 ContextErrors
   |NodeHasNoValue String
   |NodeHasNoSubForest String
   |FunctionComplete String
   |OtherError String

instance Show ApplicationLog where
  show (Include1 c) = show c
  show (NodeHasNoValue s) = "Node has no value" ++ s
  show (NodeHasNoSubForest s) = "Node has no sub-forest" ++ s
  show (OtherError str) = str


instance Error ApplicationLog where
   noMsg    = OtherError "ApplicationLog triggered!"
   strMsg s = OtherError s

{--crossCorellation_idsAtMins_interruptible
   :: Num Ord a => Seq a -> Seq a -> BudgetAwareFunc (Seq (Seq Int))
crossCorellation_idsAtMins_interruptible mask input = do--}

--recursive over crossCorellation_idsAtMins_maskBruteForce with 
--fixed mask
crossCorellation_idsAtMins_bwaaFull_fixedMask 
   :: Num Ord a => Seq a -> Seq a -> BudgetAwareFunc CorrelationAtMinsIdsTree
crossCorellation_idsAtMins_bwaaFull_fixedMask mask input = do
   b@(B{calls=c})<-get
   case ((c>0)) of
      True ->  do 
         put minusCall
         let crosscor = crossCorellation_idsAtMins mask input
         
         return $ 
            Node { value = crosscor
                  ,subForest = subF crosscor
                 }
         
      False -> 

   where
      minusCall = (\b -> b {calls = c-1}) b
      maskLengths = [(length mask) .. (length input)]
      masks c = 
      subF c = map (\m-> crossCorellation_idsAtMins_bwaaFull m c) masks


--recursive over crossCorellation_idsAtMins with 
--mask size brute-force
crossCorellation_idsAtMins_maskBruteForce 
   :: Num Ord a => Word -> Seq a -> BudgetAwareFunc [CorrelationAtMinsIdsTree]
crossCorellation_idsAtMins_maskBruteForce maskSize input = do
   b@(B{calls=c}) <- get
   
   maskSizeIsSane <- if (maskSize<=(Seq.length input))
                     then return True
                     else do throwError $ OtherError message1
                             return False

   haveCalls      <- if (c>0)
                     then return True
                     else do throwError $ Include1 $ RanOutOfCalls message2
                             return False
 
   
              
   if (maskSizeIsSane && haveCalls)
   then do crossCorellation_idsAtMins_maskBruteForce_step1 maskSize input
   else do return []


   where
      message1 = "maskSize exceeds length of input in crossCorellation_idsAtMins_maskBruteForce maskSize input"
      message2 = " in crossCorellation_idsAtMins_maskBruteForce maskSize input"

 
 
      
      

crossCorellation_idsAtMins_maskBruteForce_step1 :: Num Ord a => 
                                                        Word 
                                                     -> Seq a 
                                                     -> BudgetAwareFunc [CorrelationAtMinsIdsTree]
crossCorellation_idsAtMins_maskBruteForce_step1 maskSize input = do
   put minusCall
         let maskSlide = map (\m-> crossCorellation_idsAtMins m input) masks
         
         mapM (\c->
               return $ 
                  Node { value     = c
                        ,subForest = crossCorellation_idsAtMins_maskBruteForce_step2 maskSize c
                       }
              ) maskSlide
              
              
   where
      minusCall = (\b -> b {calls = c-1}) b
      masks = takeWhileFits maskSize input




crossCorellation_idsAtMins_maskBruteForce_step2 :: Num Ord a => 
                                                        Word 
                                                     -> Seq a 
                                                     -> BudgetAwareFunc [CorrelationAtMinsIdsTree]
crossCorellation_idsAtMins_maskBruteForce_step2 maskSize input = do
   if ((maskSize+1)>(Seq.length input))
      then do 
         throwError $ FunctionComplete message3
         return $ Seq.empty
      else if ((c-1)<0)
           then throwError $ Include1 $ RanOutOfCalls message4
           else crossCorellation_idsAtMins_maskBruteForce_step1 (maskSize+1) input
  
   where
      message3 = "reached maximum maskSize in crossCorellation_idsAtMins_maskBruteForce maskSize input"             
      message4 = " in crossCorellation_idsAtMins_maskBruteForce maskSize input"
   
   
   
   
   
   
   
   
   
   
   

















    
   
   
