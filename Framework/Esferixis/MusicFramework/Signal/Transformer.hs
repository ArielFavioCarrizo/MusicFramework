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
(>>>) producerState transformerState =
   let ipsChunkLength = spChunkLength producerState
       itsChunkLength = spChunkLength transformerState
       ipsReduceChunkLength = spReduceChunkLength producerState
       itsReduceChunkLength = spReduceChunkLength transformerState
   in case ( compare ipsChunkLength itsChunkLength ) of
      GT -> ( ipsReduceChunkLength itsChunkLength ) >>> transformerState
      LT -> producerState >>> ( itsReduceChunkLength ipsChunkLength )
      EQ -> let chunkLength = ipsChunkLength
            in ProducerState {
                 psChunkLength = chunkLength
               , psReduceChunkLength = \requestedChunkLength -> ( ipsReduceChunkLength requestedChunkLength ) >>> ( itsReduceChunkLength requestedChunkLength )
               , psPopChunk = 
                    let (inputChunk, nextInputProducerState) = psPopChunk producerState
                        (outputChunk, nextTransformerState) = tsTransform transformerState inputChunk
                    in (outputChunk, nextInputProducerState >>> nextTransformerState)
               }
