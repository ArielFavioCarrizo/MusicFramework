module Esferixis.MusicFramework.Signal
   ( SignalChunk(SignalChunk, scLength, scData) ) where

import Data.Word
import Data.Maybe

{- 
   Representación abstracta de lo que es un chunk de señal
   Representa un chunk de señal que se obtendrá efectivamente
   en un tiempo indeterminado en el futuro

   En el caso de ser un chunk silencioso, la implementación podría
   optar por no crear un chunk. Y en su lugar podría representar
   una señal cero.

   Un chunk de señal de longitud cero se interpreta como el fin del stream
-}
data SignalChunk signalData = SignalChunk { scLength :: Word64 -- Longitud del chunk
                                          , scData :: signalData -- Datos del chunk
                                          }
