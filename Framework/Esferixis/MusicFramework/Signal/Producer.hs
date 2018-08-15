module Esferixis.MusicFramework.Signal.Producer
   ( ProducerState(ProducerState, psChunkLength, psReduceChunkLength, psPopChunk) ) where

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
data ProducerState sd = ProducerState { psChunkLength :: Word64 -- Longitud de datos a extraer. Cuando es cero significa que terminó el stream.
                                      , psReduceChunkLength :: Word64 -> ProducerState sd -- Reduce la longitud de datos a extraer
                                      , psPopChunk :: (SignalChunk sd, ProducerState sd) -- Devuelve un 'chunk' de señal y la sección siguiente (Si no termina el stream). Cuando termina el stream devuelve un chunk de señal de longitud cero, indicando que termina el stream.
                                      }

instance SignalProcessorState (ProducerState sd) where
   spChunkLength = psChunkLength
   spReduceChunkLength = psReduceChunkLength

{- 
   Dado el estado de productor crea una bifurcación del
   productor, generando dos nuevos productores para cada rama.
   Devuelve el estado de cada nuevo productor en una tupla.
   Donde el primer elemento corresponde al productor izquierdo,
   y el segundo elemento corresponde al productor derecho
-}
-- FIXME: Declarar el operador de bifurcado
