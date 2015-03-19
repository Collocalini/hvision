-----------------------------------------------------------------------------
--
-- Module      :  What_i_have
-- Copyright   :
-- License     :  PublicDomain
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module What_i_have (
data_process_common_matrix_debug
) where



import qualified Data_iterators as DI
import Processors_common
import Processors2d
import Control.Monad.State
import Data.Dynamic
--import Image_loading
--import Data.Conduit
--import qualified Data.ByteString.Lazy as B



{-- common steps in data_process_range_sensitive
===============================================================================================  --}
data_process_common_matrix_debug ::  Maybe FilePath ->
                             Maybe Int ->
                             String ->
                             IO ()
data_process_common_matrix_debug  gfile rfo x =
  evalStateT (DI.iterate_all_data_v $
                                   toStringTable_matrix $
                                   stack_output_matrix $
                                   derivative_i_tapering_stacking_ready $
                                   ((\(_,r)-> map toDyn r) $ unzip $ stringToIntList_mn 1 3 x))
                                   DI.IterateData {DI.gnuplot_file = gfile, DI.repeat_frames_of_output = rfo}
  --where
  --processingPipe x =

--iterate_all_data tag_DMap $
--  map (toStringTable . stack_output) $ apply_processors_context_sensitive processors $ map adaptTo x

----------------------------------------------------------------------------------------------------

--liftIO $! evalStateT (DI.iterate_all_data_v $ processingPipe processors frame) itd
--processingPipe (PMInt_l pmi)      (avf,_) = toString $ (apply_processors_v_i pmi) $ to_grayscale_MI avf

