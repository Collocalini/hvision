-----------------------------------------------------------------------------
--
-- Module      :  Processors_common
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

module Processors_common (

Processor_data(..)

) where
import Data.Dynamic

data Processor_data = Pd Dynamic  (Dynamic -> String) --deriving (Show)

