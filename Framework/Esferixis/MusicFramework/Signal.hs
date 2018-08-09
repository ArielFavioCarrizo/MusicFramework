module Esferixis.MusicFramework.Signal
   ( SignalChunk(SignalChunk, scLength, scData) ) where

import Data.Word
import Control.Concurrent.Async
import Data.Maybe

data SignalChunk signalData = SignalChunk {
   , scLength :: Word64 -- Longitud del chunk
   , scData :: signalData -- Datos del chunk
   }
