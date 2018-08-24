{-# LANGUAGE Rank2Types #-}

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

   sc: Signal Chunk
-}
data ProducerState sc = ProducerState { psChunkLength :: Word64 -- Longitud de datos a extraer. Cuando es cero significa que terminó el stream.
                                      , psReduceChunkLength :: Word64 -> ProducerState sc -- Reduce la longitud de datos a extraer
                                      , psPopChunk :: (SignalChunk sc) => (sc, ProducerState sc) -- Devuelve un 'chunk' de señal y la sección siguiente (Si no termina el stream). Cuando termina el stream devuelve un chunk de señal de longitud cero, indicando que termina el stream.
                                      }

instance SignalProcessorState (ProducerState sc) where
   spChunkLength = psChunkLength
   spReduceChunkLength = psReduceChunkLength

{-
   Dado dos estados de productores devuelve un productor que devuelve los
   resultados de los productores de entrada en una tupla.
   Combina dos productores.
-}
psCombine :: (SignalChunk sca, SignalChunk scb) => ProducerState sca -> ProducerState scb -> ProducerState (sca, scb)
psCombine = makeSpPairConvert (\chunkLength reduceChunkLength leftProducerState rightProducerState ->
   ProducerState {
        psChunkLength = chunkLength
      , psReduceChunkLength = reduceChunkLength
      , psPopChunk = 
        let (leftChunk, nextLeftProducerState) = psPopChunk leftProducerState
            (rightChunk, nextRightProducerState) = psPopChunk rightProducerState
        in ( (leftChunk, rightChunk), psCombine nextLeftProducerState nextRightProducerState)
      } )
