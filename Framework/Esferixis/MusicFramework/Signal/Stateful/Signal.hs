{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal.Stateful.Signal
   (
     SFSignalChunkObject(sfscLength)
   , SFSignalChunk
   , SFSignalChunkIO(sfscIOInput, sfscIOOutput)
   , mkSFSignalChunkIO
   , DeallocatableSignalFrames(dsfChannels, dsfDelete)
   , dsfMonoSection
   , dsfMultichannelSection
   , DSFMonoSection(dsfmsSource, dsfmsOffset, dsfmsLength, dsfmsChannel)
   , DSFMultichannelSection(dsfmcsSource, dsfmcsOffset, dsfmcsLength)
   ) where

import Data.Word
import Data.Maybe

class SFSignalChunkObject sco where
   sfscLength :: sco -> Word64

-- Chunk de señal stateful
class (SFSignalChunkObject sc) => SFSignalChunk sc
 
-- Par E/S
data SFSignalChunkIO sc = SFSignalChunkIO {
     sfscIOInput :: (SFSignalChunk sc) => sc
   , sfscIOOutput :: (SFSignalChunk sc) => sc
   }

instance (SFSignalChunk sc) => SFSignalChunkObject (SFSignalChunkIO sc) where
   sfscLength sfscIOPair = sfscLength $ sfscIOInput sfscIOPair

-- Hace un par E/S de los chunk de señal especificados
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

class (SFSignalChunkObject dsf) => DeallocatableSignalFrames dsf f | dsf -> f where
   dsfChannels :: dsf -> Word32
   dsfFormat :: dsf -> f
   dsfDelete :: dsf -> IO ()

dsfValidateChannel :: (DeallocatableSignalFrames dsf f) => dsf -> Word32 -> a -> a
dsfValidateChannel signalFrames channel object =
   if ( channel < dsfChannels signalFrames )
      then object
      else error "Invalid channel number"

dsfValidateSection :: (SFSignalChunkObject sco) => sco -> Word64 -> Word64 -> a -> a
dsfValidateSection signalFrames offset length object =
   let srcLength = sfscLength signalFrames
   in if ( ( offset + length ) < srcLength )
         then object
         else error "Invalid interval"

dsfMonoSection :: (DeallocatableSignalFrames dsf f) => dsf -> Word32 -> Word64 -> Word64 -> DSFMonoSection dsf f
dsfMonoSection signalFrames channel offset length =
   let section =
          DSFMonoSection {
              dsfmsSource = signalFrames
            , dsfmsOffset = offset
            , dsfmsLength = length
            , dsfmsChannel = channel
            }
   in dsfValidateChannel signalFrames channel $ dsfValidateSection signalFrames offset length section

dsfMultichannelSection :: (DeallocatableSignalFrames dsf f) => dsf -> Word64 -> Word64 -> DSFMultichannelSection dsf f
dsfMultichannelSection signalFrames offset length =
   let section = 
          DSFMultichannelSection {
               dsfmcsSource = signalFrames
             , dsfmcsOffset = offset
             , dsfmcsLength = length
             }
   in dsfValidateSection signalFrames offset length section

data DSFMonoSection dsf f = DSFMonoSection {
     dsfmsSource :: (DeallocatableSignalFrames dsf f) => dsf
   , dsfmsOffset :: Word64
   , dsfmsLength :: Word64
   , dsfmsChannel :: Word32
   }

instance SFSignalChunkObject (DSFMonoSection dsf f) where
   sfscLength = dsfmsLength

data DSFMultichannelSection dsf f = DSFMultichannelSection {
     dsfmcsSource :: (DeallocatableSignalFrames dsf f) => dsf
   , dsfmcsOffset :: Word64
   , dsfmcsLength :: Word64
   }

instance SFSignalChunkObject (DSFMultichannelSection dsf f) where
   sfscLength = dsfmcsLength
