module Esferixis.MusicFramework.Signal
   ( SignalChunk(SignalChunk, scLength, scData), SignalProcessorState(spChunkLength, spReduceChunkLength) ) where

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

-- Estado de unidad de procesamiento de señal
class SignalProcessorState a where
   spChunkLength :: a -> Word64 -- Longitud de datos a procesar. Cuando es cero significa que terminó el stream.
   spReduceChunkLength :: a -> Word64 -> a -- Reduce la longitud de datos a procesar
