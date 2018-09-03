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
data TransformerState m isc osc = TransformerState {
     tsChunkLength :: Word64 -- Longitud de datos a transformar
   , tsReduceChunkLength :: Word64 -> TransformerState m isc osc -- Reduce la longitud de datos a transformar al valor especificado, devolviendo un nuevo estado del transformador
   , tsTransform :: (Monad m, SignalChunk m isc, SignalChunk m osc) => isc -> m (osc, TransformerState m isc osc) -- Recibe el chunk de señal de entrada, devuelve el chunk de salida y el siguiente estado del transformador. Cuando recibe un chunk de señal de longitud cero, significa que se termina el stream de entrada.
   }

instance SignalProcessorState (TransformerState m isc osc) where
   spChunkLength = tsChunkLength
   spReduceChunkLength = tsReduceChunkLength

{-
   Dado el estado de productor y el estado de transformador
   especificados, devuelve un estado de productor compuesto

   Tipos

   isc: Input Signal Chunk
   osc: Output Signal Chunk
-}
(|>>) :: (Monad m, SignalChunk m isc, SignalChunk m osc) => ProducerState m isc -> TransformerState m isc osc -> ProducerState m osc
(|>>) = makeSpPairConvert $ \chunkLength reduceChunkLength producerState transformerState ->
   ProducerState {
        psChunkLength = chunkLength
      , psReduceChunkLength = reduceChunkLength
      , psPopChunk = do
           (inputChunk, nextInputProducerState) <- psPopChunk producerState
           (outputChunk, nextTransformerState) <- tsTransform transformerState inputChunk
           return (outputChunk, nextInputProducerState |>> nextTransformerState)
      }

{-
   Dado el estado del transformador primero y el estado de transformador
   segundo, devuelve un estado de trasformador compuesto en serie

   Tipos

   isc: Input Signal Chunk
   msc: Medium Signal Chunk
   osc: Output Signal Chunk
-}
(>>>) :: (Monad m, SignalChunk m isc, SignalChunk m msc, SignalChunk m osc) => TransformerState m isc msc -> TransformerState m msc osc -> TransformerState m isc osc
(>>>) = makeSpPairConvert $ \chunkLength reduceChunkLength leftTransformerState rightTransformerState ->
   TransformerState {
        tsChunkLength = chunkLength
      , tsReduceChunkLength = reduceChunkLength
      , tsTransform = \inputChunk -> do
           (intermediateChunk, nextLeftTransformerState) <- tsTransform leftTransformerState inputChunk
           (outputChunk, nextRightTransformerState) <- tsTransform rightTransformerState intermediateChunk
           return ( outputChunk, nextLeftTransformerState >>> nextRightTransformerState )
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
(&&&) :: (Monad m, SignalChunk m isc, SignalChunk m losc, SignalChunk m rosc) => TransformerState m isc losc -> TransformerState m isc rosc -> TransformerState m isc (losc, rosc)
(&&&) = makeSpPairConvert $ \chunkLength reduceChunkLength leftTransformerState rightTransformerState ->
   TransformerState {
        tsChunkLength = chunkLength
      , tsReduceChunkLength = reduceChunkLength
      , tsTransform = \inputChunk -> do
           (leftInputChunk, rightInputChunk) <- scSplitRef inputChunk
           (leftOutputChunk, nextLeftTransformerState) <- tsTransform leftTransformerState leftInputChunk
           (rightOutputChunk, nextRightTransformerState) <- tsTransform rightTransformerState rightInputChunk
           return ( (leftOutputChunk, rightOutputChunk), nextLeftTransformerState &&& nextRightTransformerState )
      }
