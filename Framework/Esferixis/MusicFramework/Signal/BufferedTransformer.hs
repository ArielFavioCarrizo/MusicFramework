{-# LANGUAGE Rank2Types #-}

module Esferixis.MusicFramework.Signal.BufferedTransformer
   ( BufferedTransformerOutputState(btsMaxPopChunkLength, btsPopChunk), BufferedTransformerInputState(btsPushChunkLength, btsReducePushChunkLength, btsPushChunk), BufferedTransformerState(btsOutput, btsInput) ) where

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
data BufferedTransformerOutputState m isc osc = BufferedTransformerOutputState {
     btsMaxPopChunkLength :: Word64 -- Longitud de datos a extraer
   , btsPopChunk :: (Monad m, SignalChunk m isc, SignalChunk m osc) => Word64 -> m (osc, BufferedTransformerState m isc osc) -- Extracción de chunk con el tamaño especificado
   }

data BufferedTransformerInputState m isc osc = BufferedTransformerInputState {
     btsPushChunkLength :: Word64
   , btsReducePushChunkLength :: Word64 -> m ( BufferedTransformerState m isc osc )
   , btsPushChunk :: (Monad m, SignalChunk m isc, SignalChunk m isc) => isc -> m (BufferedTransformerState m isc osc) -- Recibe el chunk de señal de entrada. Si recibe un chunk de señal de longitud cero, significa que se terminó el stream de entrada.
   }

data BufferedTransformerState m isc osc = BufferedTransformerState {
     btsOutput :: Maybe (BufferedTransformerOutputState m isc osc)
   , btsInput :: Maybe (BufferedTransformerInputState m isc osc)
   }

newBufferedTransformer :: (Monad m, SignalChunk m isc, SignalChunk m osc) => TransformerState m isc osc -> m (BufferedTransformerState m isc osc)
newBufferedTransformer transformerState = do
   initialAccumulatedBuffer <- scEmpty
   makeBufferedTransformerState False initialAccumulatedBuffer transformerState

makeBufferedTransformerState :: (Monad m, SignalChunk m isc, SignalChunk m osc) => Bool -> osc -> TransformerState m isc osc -> m ( BufferedTransformerState m isc osc)
makeBufferedTransformerState isTerminated accumulatedSignalChunk transformerState = do
   let pushChunkLength = tsChunkLength transformerState
   accumulatedSignalChunkIsEmpty <- scIsEmpty accumulatedSignalChunk
   maxPopChunkLength <- scLength accumulatedSignalChunk
   return $ BufferedTransformerState {
         btsOutput =
            if ( (not isTerminated ) && accumulatedSignalChunkIsEmpty )
               then Nothing
               else
                  Just ( BufferedTransformerOutputState {
                       btsMaxPopChunkLength = maxPopChunkLength
                     , btsPopChunk = \requestedLength -> do
                          nextAccumulatedChunk <-
                             if ( requestedLength <= maxPopChunkLength )
                                then scSection accumulatedSignalChunk (requestedLength+1) (maxPopChunkLength-requestedLength)
                                else error "Invalid chunk size"
                          nextState <- makeBufferedTransformerState isTerminated nextAccumulatedChunk transformerState
                          resultSection <- scSection accumulatedSignalChunk 0 requestedLength
                          return (resultSection, nextState)
                     } )
       , btsInput =
            if ( isTerminated )
               then Nothing
               else
                  Just BufferedTransformerInputState {  
                       btsPushChunkLength = pushChunkLength
                     , btsReducePushChunkLength = \requestedLength -> makeBufferedTransformerState isTerminated accumulatedSignalChunk (tsReduceChunkLength transformerState requestedLength)
                     , btsPushChunk = \inputChunk -> do
                          (transformedInputChunk, nextTransformerState) <- tsTransform transformerState inputChunk
                          nextAccumulatedChunk <- scAppend accumulatedSignalChunk transformedInputChunk
                          nextIsTerminated <- scIsEmpty inputChunk
                          makeBufferedTransformerState nextIsTerminated nextAccumulatedChunk nextTransformerState
                     }
       }
