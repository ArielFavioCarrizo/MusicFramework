{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal.Operations.Signal
   ( 
     SFSignalChunk(SFSignalChunk, scLength, scConsume)
   , SFSignalChunkIO(sfscIOInput, sfscIOOutput)
   , mkSFSignalChunkIO
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
