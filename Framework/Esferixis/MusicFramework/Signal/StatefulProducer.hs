module Esferixis.MusicFramework.Signal.StatefulProducer
   ( StatefulProducer(StatefulProducer, spActions, spMethods)
   , StatefulProducerMethods (StatefulProducerMethods, spTick, spClose)
   , spStart ) where

import Data.Word
import Data.Maybe
import Esferixis.MusicFramework.Signal.Producer
import Control.Concurrent.Async
import Control.Monad

data StatefulProducerMethods dataChunk = StatefulProducerMethods { spTick :: Word64 -> IO dataChunk -- Produce un chunk de datos de la longitud especificada
                                                                 , spClose :: IO () -- Cierra el productor
                                                                 }

data StatefulProducer dataChunk = StatefulProducer { spActions :: [(Word64, IO ())] -- Acciones por longitud de frames de vigencia
                                                   , spMethods :: StatefulProducerMethods dataChunk -- Métodos imperativos
                                                   }

-- Comienza el funcionamiento del productor stateful devolviendo el primer estado si tiene acciones
spStart :: StatefulProducer dataChunk -> IO ( Maybe ( ProducerState dataChunk ) )
spStart statefulProducer = do
   result <- ( spMakeProducerStateDoingAction ( return () ) statefulProducer )
   case result of
      Just firstState -> return ( Just firstState )
      Nothing -> do
         spClose ( spMethods statefulProducer )
         return Nothing

spMakeProducerStateDoingAction :: IO () -> StatefulProducer dataChunk -> IO ( Maybe ( ProducerState dataChunk ) )
spMakeProducerStateDoingAction waitPrevious statefulProducer = do
   case ( spActions statefulProducer ) of
      [] -> return Nothing
      ((duration, currentSpAction):nextSpActions) -> do
         let waitPrevious_next = async ( do
             waitPrevious
             currentSpAction )

         return ( Just ( spMakeProducerStateFromRemainingFrames waitPrevious duration statefulProducer { spActions = nextSpActions } ) )

spMakeProducerStateFromRemainingFrames :: IO () -> Word64 -> StatefulProducer dataChunk -> ProducerState dataChunk
spMakeProducerStateFromRemainingFrames waitPrevious remainingFrames statefulProducer =
   let methods = spMethods statefulProducer

   in ProducerState { 
      psMaxPopChunkLength = remainingFrames
      , psPopChunk = \requestedFrames -> do
         resultDataChunk <- async ( do
            waitPrevious
            dataChunk <- spTick methods requestedFrames
            return dataChunk )

         let waitPrevious_next = void ( wait resultDataChunk )

         nextProducerState <- if ( requestedFrames == remainingFrames )
                                 then ( spMakeProducerStateDoingAction waitPrevious_next statefulProducer )
                                 else return ( Just ( spMakeProducerStateFromRemainingFrames waitPrevious_next ( remainingFrames - requestedFrames ) statefulProducer ) )

         return ( Just resultDataChunk, nextProducerState )
      , psCloseSourceStream = async (do
         waitPrevious
         spClose methods )
      }
