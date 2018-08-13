module Esferixis.MusicFramework.Signal.Transformer
   ( TransformerState(TransformerState, tsMaxChunkLength, tsTransform) ) where

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
data TransformerState isd osd = TransformerState { tsMaxChunkLength :: Word64 -- Máxima longitud de datos que se puede transformar
                                                 , tsTransform :: SignalChunk isd -> (SignalChunk osd, TransformerState isd osd) -- Recibe el chunk de señal de entrada, devuelve el chunk de salida y el siguiente estado del transformador. Cuando recibe un chunk de señal de longitud cero, significa que se termina el stream de entrada.
                                                 }

{-
    Dado el estado de productor y el estado de transformador
    especificados, devuelve un estado de productor compuesto

    Tipos

    isd: Input Signal Data
    osd: Output Signal Data
-}
(>>>) :: ProducerState isd -> TransformerState isd osd -> ProducerState osd
(>>>) producerState transformerState =
   ProducerState { psMaxChunkLength = min ( psMaxChunkLength producerState ) ( tsMaxChunkLength transformerState )
                 , psPopChunk = \requestedChunkLength ->
                    let (inputChunk, nextInputProducerState) = psPopChunk producerState requestedChunkLength
                        (outputChunk, nextTransformerState) = tsTransform transformerState inputChunk
                    in (outputChunk, nextInputProducerState >>> nextTransformerState)
                 }
