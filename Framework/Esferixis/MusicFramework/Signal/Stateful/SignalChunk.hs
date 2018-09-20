{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Signal.Stateful.SignalChunk
   (
     SFSignalChunk(sfscLength)
   , SFSignalChunkIO(sfscIOInput, sfscIOOutput)
   , sfscIOLength
   ) where

import Data.Word
import Data.Maybe

-- Chunk de señal stateful
class SFSignalChunk sc where
   sfscLength :: sc -> Word64
 
-- Par E/S
data SFSignalChunkIO sc = SFSignalChunkIO {
     sfscIOInput :: (SFSignalChunk sc) => sc
   , sfscIOOutput :: (SFSignalChunk sc) => sc
   }

mkSFSignalChunkIO :: (SFSignalChunk sc) => sc -> sc -> SFSignalChunkIO sc 
mkSFSignalChunkIO inputChunk outputChunk =
   if ( (sfscLength inputChunk) == (sfscLength outputChunk ) )
      then
         SFSignalChunkIO {
              sfscIOInput = inputChunk
            , sfscIOOutput = outputChunk
            }
      else
         error "Chunk size mismatch"

sfscIOLength :: (SFSignalChunk sc) => SFSignalChunkIO sc -> Word64
sfscIOLength sfscIOPair = sfscLength $ sfscIOInput sfscIOPair
