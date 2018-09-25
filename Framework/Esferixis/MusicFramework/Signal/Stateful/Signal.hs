{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal.Stateful.Signal
   ( 
     SFSignalChunk
   , SFSignalChunkIO(sfscIOInput, sfscIOOutput)
   , mkSFSignalChunkIO
   , DeallocatableSignalFrames(dsfChannels, dsfDelete)
   ) where

import Data.Word
import Data.Maybe

import Esferixis.MusicFramework.Signal

-- Chunk de señal stateful
class (Sectionable sc) => SFSignalChunk sc
 
-- Par E/S
data SFSignalChunkIO sc = SFSignalChunkIO {
     sfscIOInput :: (SFSignalChunk sc) => sc
   , sfscIOOutput :: (SFSignalChunk sc) => sc
   }

instance (SFSignalChunk sc) => Sectionable (SFSignalChunkIO sc) where
   sLength sfscIOPair = sLength $ sfscIOInput sfscIOPair

-- Hace un par E/S de los chunk de señal especificados
mkSFSignalChunkIO :: (SFSignalChunk sc) => sc -> sc -> SFSignalChunkIO sc 
mkSFSignalChunkIO inputChunk outputChunk =
   if ( (sLength inputChunk) == (sLength outputChunk ) )
      then
         SFSignalChunkIO {
              sfscIOInput = inputChunk
            , sfscIOOutput = outputChunk
            }
      else
         error "Chunk size mismatch"

class (Sectionable dsf) => DeallocatableSignalFrames dsf f | dsf -> f where
   dsfChannels :: dsf -> Word32
   dsfFormat :: dsf -> f
   dsfDelete :: dsf -> IO ()

data DSFChannel dsf f = DSFChannel {
     dsfcSource :: (DeallocatableSignalFrames dsf f) => dsf
   , dsfcChannel :: Word32
   }

instance (DeallocatableSignalFrames dsf f) => Sectionable (DSFChannel dsf f) where
   sLength dsfChannel = sLength $ dsfcSource dsfChannel

dsfChannel :: (DeallocatableSignalFrames dsf f) => dsf -> Word32 -> DSFChannel dsf f
dsfChannel signalFrames nChannel =
   if ( nChannel < dsfChannels signalFrames )
      then
         DSFChannel {
              dsfcSource = signalFrames
            , dsfcChannel = nChannel
            }
      else error "Invalid channel number"
