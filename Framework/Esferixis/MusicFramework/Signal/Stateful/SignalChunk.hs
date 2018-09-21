{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Signal.Stateful.SignalChunk
   (
     SFSignalObject(sfscLength)
   , SFSignalChunk
   , SFSignalChunkIO(sfscIOInput, sfscIOOutput)
   , mkSFSignalChunkIO
   ) where

import Data.Word
import Data.Maybe

class SFSignalObject sco where
   sfscLength :: sco -> Word64

-- Chunk de señal stateful
class (SFSignalObject sc) => SFSignalChunk sc
 
-- Par E/S
data SFSignalChunkIO sc = SFSignalChunkIO {
     sfscIOInput :: (SFSignalChunk sc) => sc
   , sfscIOOutput :: (SFSignalChunk sc) => sc
   }

instance (SFSignalChunk sc) => SFSignalObject (SFSignalChunkIO sc) where
   sfscLength sfscIOPair = sfscLength $ sfscIOInput sfscIOPair

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
