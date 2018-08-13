module Esferixis.MusicFramework.Signal.Producer
   ( ProducerState(ProducerState, psMaxChunkLength, psPopChunk) ) where

import Data.Word
import Data.Maybe
import Esferixis.MusicFramework.Signal

{- 
   Representación abstracta de lo que es un estado de productor.
   Representa un estado del productor que se obtendrá efectivamente
   en un tiempo indeterminado en el futuro.

   Tipos

   sd: Signal Data
-}
data ProducerState sd = ProducerState { psMaxChunkLength :: Word64 -- Máxima longitud de datos que se puede extraer
                                      , psPopChunk :: Word64 -> (SignalChunk sd, ProducerState sd) -- Recibe la longitud deseada y devuelve un 'chunk' de señal y la sección siguiente (Si no termina el stream). Cuando termina el stream devuelve un chunk de señal de longitud cero, indicando que termina el stream.
                                      }
