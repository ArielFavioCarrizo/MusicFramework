{-# LANGUAGE Rank2Types #-}

module Esferixis.MusicFramework.Signal.BufferedTransformer
   ( BufferedTransformerOutputState(btsMaxPopChunkLength, btsPopChunk), BufferedTransformerInputState(btsPushChunkLength, btsReducePushChunkLength, btsPushChunk),  BufferedTransformerState(btsOutput, btsInput) ) where

import Data.Word
import Data.Maybe
import Esferixis.MusicFramework.Signal
import Esferixis.MusicFramework.Signal.Producer
import Esferixis.MusicFramework.Signal.Transformer

{- 
   Representación abstracta de lo que es un transformador con buffering.
   Representa un estado del transformador que se obtendrá efectivamente
   en un tiempo indeterminado en el futuro.

   Tipos

   isc: Input Signal Chunk
   osc: Output Signal Chunk
-}
data BufferedTransformerOutputState isc osc = BufferedTransformerOutputState {
     btsMaxPopChunkLength :: Word64 -- Longitud de datos a extraer
   , btsPopChunk :: (SignalChunk osc) => Word64 -> (osc, BufferedTransformerState isc osc) -- Extracción de chunk con el tamaño especificado
   }

data BufferedTransformerInputState isc osc = BufferedTransformerInputState {
     btsPushChunkLength :: Word64
   , btsReducePushChunkLength :: Word64 -> BufferedTransformerState isc osc
   , btsPushChunk :: (SignalChunk isc) => isc -> BufferedTransformerState isc osc -- Recibe el chunk de señal de entrada. Si recibe un chunk de señal de longitud cero, significa que se terminó el stream de entrada.
   }

data BufferedTransformerState isc osc = BufferedTransformerState {
     btsOutput :: Maybe (BufferedTransformerOutputState isc osc)
   , btsInput :: Maybe (BufferedTransformerInputState isc osc)
   }

newBufferedTransformer :: (SignalChunk osc) => TransformerState isc osc -> BufferedTransformerState isc osc
newBufferedTransformer transformerState = makeBufferedTransformerState False Nothing transformerState

makeBufferedTransformerState :: (SignalChunk osc) => Bool -> Maybe osc -> TransformerState isc osc -> BufferedTransformerState isc osc
makeBufferedTransformerState isTerminated accumulatedSignalChunk_opt transformerState =
   let pushChunkLength = tsChunkLength transformerState
   in BufferedTransformerState {
         btsOutput = do
            accumulatedSignalChunk <- accumulatedSignalChunk_opt
            let maxPopChunkLength = scLength accumulatedSignalChunk
            Just ( BufferedTransformerOutputState {
                 btsMaxPopChunkLength = maxPopChunkLength
               , btsPopChunk = \requestedLength ->
                    let nextAccumulatedSignalChunk =
                           case ( compare requestedLength maxPopChunkLength ) of
                              LT -> Just ( scSection accumulatedSignalChunk (requestedLength+1) (maxPopChunkLength-requestedLength) )
                              EQ -> if ( isTerminated )
                                       then Just scEmpty
                                       else Nothing
                              GT -> error "Invalid chunk size"
                    in (scSection accumulatedSignalChunk 0 requestedLength, makeBufferedTransformerState isTerminated nextAccumulatedSignalChunk transformerState)
               } )
       , btsInput =
            if ( isTerminated )
               then Nothing
               else
                  Just BufferedTransformerInputState {  
                       btsPushChunkLength = pushChunkLength
                     , btsReducePushChunkLength = \requestedLength -> makeBufferedTransformerState isTerminated accumulatedSignalChunk_opt (tsReduceChunkLength transformerState requestedLength)
                     , btsPushChunk = \inputChunk ->
                          let (transformedInputChunk, nextTransformerState) = tsTransform transformerState inputChunk
                              nextAccumulatedChunk = scAppend (scFromMaybe accumulatedSignalChunk_opt) transformedInputChunk
                              nextIsTerminated = scIsEmpty inputChunk
                          in makeBufferedTransformerState nextIsTerminated ( scToMaybe nextAccumulatedChunk ) nextTransformerState
                     }
       }
