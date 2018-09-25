{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal.Stateful.Signal
   ( 
     SFSignalChunk
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

dsfValidateChannel :: (DeallocatableSignalFrames dsf f) => dsf -> Word32 -> a -> a
dsfValidateChannel signalFrames channel object =
   if ( channel < dsfChannels signalFrames )
      then object
      else error "Invalid channel number"

dsfMonoSection :: (DeallocatableSignalFrames dsf f) => dsf -> Word32 -> Word64 -> Word64 -> DSFMonoSection dsf f
dsfMonoSection signalFrames channel offset length =
   let section =
          DSFMonoSection {
              dsfmsSource = signalFrames
            , dsfmsOffset = offset
            , dsfmsLength = length
            , dsfmsChannel = channel
            }
   in dsfValidateChannel signalFrames channel $ validateSection signalFrames offset length section

dsfMultichannelSection :: (DeallocatableSignalFrames dsf f) => dsf -> Word64 -> Word64 -> DSFMultichannelSection dsf f
dsfMultichannelSection = mkSectionFun $ \signalFrames offset length ->
   DSFMultichannelSection {
        dsfmcsSource = signalFrames
      , dsfmcsOffset = offset
      , dsfmcsLength = length
      }

data DSFMonoSection dsf f = DSFMonoSection {
     dsfmsSource :: (DeallocatableSignalFrames dsf f) => dsf
   , dsfmsOffset :: Word64
   , dsfmsLength :: Word64
   , dsfmsChannel :: Word32
   }

instance Sectionable (DSFMonoSection dsf f) where
   sLength = dsfmsLength

instance (DeallocatableSignalFrames dsf f) => MonomorphicSectionable (DSFMonoSection dsf f) where
   sSection = mkSectionFun $ \srcSection offset length ->
      dsfMonoSection (dsfmsSource srcSection) (dsfmsChannel srcSection) ( (dsfmsOffset srcSection) + offset ) length

data DSFMultichannelSection dsf f = DSFMultichannelSection {
     dsfmcsSource :: (DeallocatableSignalFrames dsf f) => dsf
   , dsfmcsOffset :: Word64
   , dsfmcsLength :: Word64
   }

instance Sectionable (DSFMultichannelSection dsf f) where
   sLength = dsfmcsLength

instance (DeallocatableSignalFrames dsf f) => MonomorphicSectionable (DSFMultichannelSection dsf f) where
   sSection = mkSectionFun $ \srcSection offset length ->
      dsfMultichannelSection (dsfmcsSource srcSection) ( (dsfmcsOffset srcSection) + offset ) length
