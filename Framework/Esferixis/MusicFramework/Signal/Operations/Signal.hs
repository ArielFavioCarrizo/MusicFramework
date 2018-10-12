{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal.Operations.Signal
   ( 
     SFSignalChunk(SFSignalChunk, scLength, scConsume)
   , SFSignalChunkIO(sfscIOInput, sfscIOOutput)
   , mkSFSignalChunkIO
   , SFSignalUnitSt(sfsuMaxFrames, sfsuPushTickOp, sfsuTerminate)
   , sfsuDoTicksBatch
   ) where

import Data.Word
import Data.Maybe
import Esferixis.Control.Concurrency.AsyncIO

import Esferixis.MusicFramework.Signal.Misc

-- Chunk de señal stateful.
data SFSignalChunk sc = SFSignalChunk {
     scLength :: Word64 -- Longitud del chunk
   , scConsume :: (sc -> AsyncIO ()) -> AsyncIO () -- Consume el chunk con un callback, que recibe la referencia del chunk y le aplica operaciones
   }
 
-- Par E/S
data SFSignalChunkIO sc= SFSignalChunkIO {
     sfscIOInput :: SFSignalChunk sc
   , sfscIOOutput :: SFSignalChunk sc
   }

-- Hace un par E/S de los chunk de señal especificados
mkSFSignalChunkIO :: SFSignalChunk sc -> SFSignalChunk sc -> SFSignalChunkIO sc 
mkSFSignalChunkIO inputChunk outputChunk =
   if ( (scLength inputChunk) == (scLength outputChunk ) )
      then
         SFSignalChunkIO {
              sfscIOInput = inputChunk
            , sfscIOOutput = outputChunk
            }
      else
         error "Chunk size mismatch"

-- Definición de unidad de señal
class SFSignalUnitSt su ti | su -> ti where
   sfsuMaxFrames :: su -> Word64
   sfsuPushTickOp :: su -> ti -> AsyncIO ( Maybe ( su ) )
   sfsuTerminate :: su -> AsyncIO ()

{-
   Devuelve una acción que toma el estado de la unidad de procesamiento de señal
   y la lista de entrada de ticks.
   Realiza los ticks.
   
   La acción devuelve el próximo estado.
-}
sfsuDoTicksBatch :: (SFSignalUnitSt su ti) => su -> [ti] -> AsyncIO ( Maybe su )
sfsuDoTicksBatch unitSt [] = return $ Just $ unitSt
sfsuDoTicksBatch unitSt (tickIn:nextTickIns) = do
   mnextUnitSt <- sfsuPushTickOp unitSt tickIn
   case mnextUnitSt of
      Just nextUnitSt -> sfsuDoTicksBatch nextUnitSt nextTickIns
      Nothing ->
         if ( null nextTickIns )
            then return $ Nothing
            else fail "Unexpected signal processor unit termination"
