{-# LANGUAGE Rank2Types #-}

module Esferixis.MusicFramework.Signal.Transformer
   ( TransformerState(TransformerState, tsChunkLength, tsReduceChunkLength, tsTransform) ) where

import Data.Word
import Data.Maybe
import Esferixis.MusicFramework.Signal
import Esferixis.MusicFramework.Signal.Producer

{- 
   Representación abstracta de lo que es un transformador.
   Representa un estado del transformador que se obtendrá efectivamente
   en un tiempo indeterminado en el futuro.

   Tipos

   isc: Input Signal Chunk
   osc: Output Signal Chunk
-}
data TransformerState isc osc = TransformerState {
     tsChunkLength :: Word64 -- Longitud de datos a transformar
   , tsReduceChunkLength :: Word64 -> TransformerState isc osc -- Reduce la longitud de datos a transformar al valor especificado, devolviendo un nuevo estado del transformador
   , tsTransform :: (SignalChunk isc, SignalChunk osc) => isc -> (osc, TransformerState isc osc) -- Recibe el chunk de señal de entrada, devuelve el chunk de salida y el siguiente estado del transformador. Cuando recibe un chunk de señal de longitud cero, significa que se termina el stream de entrada.
   }

instance SignalProcessorState (TransformerState isc osc) where
   spChunkLength = tsChunkLength
   spReduceChunkLength = tsReduceChunkLength

{-
   Dado el estado de productor y el estado de transformador
   especificados, devuelve un estado de productor compuesto

   Tipos

   isc: Input Signal Chunk
   osc: Output Signal Chunk
-}
(|>>) :: (SignalChunk isc, SignalChunk osc) => ProducerState isc -> TransformerState isc osc -> ProducerState osc
(|>>) = makeSpPairConvert $ \chunkLength reduceChunkLength producerState transformerState ->
   ProducerState {
        psChunkLength = chunkLength
      , psReduceChunkLength = reduceChunkLength
      , psPopChunk = 
        let (inputChunk, nextInputProducerState) = psPopChunk producerState
            (outputChunk, nextTransformerState) = tsTransform transformerState inputChunk
        in (outputChunk, nextInputProducerState |>> nextTransformerState)
      }

{-
   Dado el estado del transformador primero y el estado de transformador
   segundo, devuelve un estado de trasformador compuesto en serie

   Tipos

   isc: Input Signal Chunk
   msc: Medium Signal Chunk
   osc: Output Signal Chunk
-}
(>>>) :: (SignalChunk isc, SignalChunk msc, SignalChunk osc) => TransformerState isc msc -> TransformerState msc osc -> TransformerState isc osc
(>>>) = makeSpPairConvert $ \chunkLength reduceChunkLength leftTransformerState rightTransformerState ->
   TransformerState {
        tsChunkLength = chunkLength
      , tsReduceChunkLength = reduceChunkLength
      , tsTransform = \inputChunk ->
           let (intermediateChunk, nextLeftTransformerState) = tsTransform leftTransformerState inputChunk
               (outputChunk, nextRightTransformerState) = tsTransform rightTransformerState intermediateChunk
           in ( outputChunk, nextLeftTransformerState >>> nextRightTransformerState )
      }

{- 
   Dado dos transformadores genera un transformador que toma la entrada y la divide entre los
   dos transformadores, y combina sus salidas en una tupla.
   Donde el primer elemento corresponde con el transformador izquierdo, y el segundo con el derecho.

   Tipos:

   isc: Input Signal Chunk
   losc: Left Output Signal Chunk
   rosc: Right Output Signal Chunk
-}
(&&&) :: (SignalChunk isc, SignalChunk losc, SignalChunk rosc) => TransformerState isc losc -> TransformerState isc rosc -> TransformerState isc (losc, rosc)
(&&&) = makeSpPairConvert $ \chunkLength reduceChunkLength leftTransformerState rightTransformerState ->
   TransformerState {
        tsChunkLength = chunkLength
      , tsReduceChunkLength = reduceChunkLength
      , tsTransform = \inputChunk ->
           let (leftInputChunk, rightInputChunk) = scSplitRef inputChunk
               (leftOutputChunk, nextLeftTransformerState) = tsTransform leftTransformerState leftInputChunk
               (rightOutputChunk, nextRightTransformerState) = tsTransform rightTransformerState rightInputChunk
               combinedOutputSignalChunk = (leftOutputChunk, rightOutputChunk)
           in ( combinedOutputSignalChunk, nextLeftTransformerState &&& nextRightTransformerState )
      }
