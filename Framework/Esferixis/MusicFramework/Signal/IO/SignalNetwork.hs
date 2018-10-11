{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFramework.Signal.IO.SignalNetwork(
   ) where

import Data.Word
import Data.Maybe
import Data.Functor
import Type.Reflection
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise

data NSignalChunk sc ps = NSignalChunk {
     nscLength :: Word64
   , nscConsumers :: Word64
   , nscGen :: sc -> AsyncIO ( ps )
   }
