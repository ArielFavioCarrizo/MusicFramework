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

   isd: Input Signal Data
   osd: Output Signal Data
-}
data TransformerState isd osd = TransformerState { tsChunkLength :: Word64 -- Longitud de datos a transformar
                                                 , tsReduceChunkLength :: Word64 -> TransformerState isd osd -- Reduce la longitud de datos a transformar al valor especificado, devolviendo un nuevo estado del transformador
                                                 , tsTransform :: SignalChunk isd -> (SignalChunk osd, TransformerState isd osd) -- Recibe el chunk de señal de entrada, devuelve el chunk de salida y el siguiente estado del transformador. Cuando recibe un chunk de señal de longitud cero, significa que se termina el stream de entrada.
                                                 }

instance SignalProcessorState (TransformerState isd osd) where
   spChunkLength = tsChunkLength
   spReduceChunkLength = tsReduceChunkLength

{-
   Dado el estado de productor y el estado de transformador
   especificados, devuelve un estado de productor compuesto

   Tipos

   isd: Input Signal Data
   osd: Output Signal Data
-}
(>>>) :: ProducerState isd -> TransformerState isd osd -> ProducerState osd
(>>>) = makeSpPairConvert (\chunkLength reduceChunkLength producerState transformerState ->
   ProducerState {
        psChunkLength = chunkLength
      , psReduceChunkLength = reduceChunkLength
      , psPopChunk = 
        let (inputChunk, nextInputProducerState) = psPopChunk producerState
            (outputChunk, nextTransformerState) = tsTransform transformerState inputChunk
        in (outputChunk, nextInputProducerState >>> nextTransformerState)
      } )

{- 
   Dado dos transformadores genera un transformador que toma la entrada y la divide entre los
   dos transformadores, y combina sus salidas en una tupla.
   Donde el primer elemento corresponde con el transformador izquierdo, y el segundo con el derecho.

   Tipos:

   isd: Input Signal data
   losd: Left Output Signal Data
   rosd: Right Output Signal Data
-}
(&&&) :: TransformerState isd losd -> TransformerState isd rosd -> TransformerState isd (losd, rosd)
(&&&) = makeSpPairConvert (\chunkLength reduceChunkLength leftTransformerState rightTransformerState ->
   TransformerState {
        tsChunkLength = chunkLength
      , tsReduceChunkLength = reduceChunkLength
      , tsTransform = \inputChunk ->
           let (leftOutputChunk, nextLeftTransformerState) = tsTransform leftTransformerState inputChunk
               (rightOutputChunk, nextRightTransformerState) = tsTransform rightTransformerState inputChunk
               combinedOutputSignalChunk = SignalChunk {
                    scLength = chunkLength
                  , scData = ( scData leftOutputChunk, scData rightOutputChunk )
                  }
           in ( combinedOutputSignalChunk, nextLeftTransformerState &&& nextRightTransformerState )
      } )

{-
   Dado dos transformadores genera un transformador donde la señal se combina con la salida del transformador derecho,
   y luego se inyecta en el transformador izquierdo, después sale como salida y también se inyecta en el transformador derecho

   FIXME: Implementar un buffer de señal
-}
{-
loop :: TransformerState (isd, rosd) losd -> TransformerState ( losd, rosd ) -> TransformerState isd losd
loop = makeSpPairConvert (\chunkLength reduceChunkLength leftTransformerState rightTransformerState ->
   TransformerState {
        tsChunkLength = chunkLength
      , tsReduceChunkLength = reduceChunkLength
      , tsTransform = \inputChunk -> -- FIXME: Implementar
      } )
-}
