-- |
-- Module      :  Esferixis.MusicFramework.Music
-- Copyright   :  (c) 2019 Ariel Favio Carrizo
-- License     :  BSD-3-Clause
-- Stability   : experimental
-- Portability : ghc

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Esferixis.MusicFramework.Music where

import qualified Esferixis.MusicFramework.MIDI as MIDI
import Data.Proxy
import Data.Array
import Data.Dynamic

class (Typeable e) => Instrument e where
   transposeIEvent :: e -> Double -> e -- Transpose pitch

{-
   Instrument state

   e: Instrument's event
-}
data InstrumentState e = InstrumentState {
   instrumentEventToMIDI :: (Instrument e) => e -> (InstrumentState e, MIDI.ChannelMsg)
   }

-- Instrument entry
data InstrumentEntry where
   InstrumentEntry :: (Instrument e) => InstrumentId e -> InstrumentState e -> InstrumentEntry

data InstrumentId e = InstrumentId Int


-- Musical context monad
data MContext a where
   --MContextMkInstrument :: MContext s -> InstrumentState e -> ( MContext s -> InstrumentId e 
   MContextReturn :: [InstrumentEntry] -> a -> MContext a

-- Instrument map
data InstrumentMap = InstrumentMap (Array Int Dynamic)

--mkInstrumentMap :: [InstrumentEntry] -> InstrumentMap
--mkInstrumentMap 

data IEvent where
   IEvent :: (Instrument e) => InstrumentId e -> e -> IEvent

-- Musical event
data MEvent e =
   IE e | -- Instrument event
   TD Double -- Time delta

-- Music DSL
data Music =
   ME (MEvent IEvent) | -- Instrument event
   Music :+: Music | -- Sequential composition
   Music :=: Music -- Parallel composition

-- Transpose music's pitch
transpose :: Music -> Double -> Music
transpose (ME (IE (IEvent instrumentId iEvent))) pitchDelta  =
   ME $ IE $ IEvent instrumentId $ transposeIEvent iEvent pitchDelta
transpose music pitchDelta =
   music

-- Makes a music value from the given instrument id and instrument event
i :: (Instrument e) => InstrumentId e -> e -> Music
i instrumentId ievent = ME $ IE $ IEvent instrumentId ievent

-- Makes a music value that represents a timestamp delta from raw timestamp delta value
td :: Double -> Music
td timeDelta =
   ME $ TD timeDelta

-- Converts music to a list of events
musicToEvents :: Music -> [MEvent IEvent]
musicToEvents (ME mevent) =
   [mevent]
musicToEvents ( leftMusic :+: rightMusic ) =
   ( musicToEvents leftMusic ) ++ ( musicToEvents rightMusic )
musicToEvents ( leftMusic :=: rightMusic ) =
   ( musicToEvents leftMusic ) `mePar` ( musicToEvents rightMusic )

-- Events paralellization
mePar :: [MEvent e] -> [MEvent e] -> [MEvent e]
mePar cmds [] = cmds
mePar [] cmds = cmds
mePar (TD leftTimeDelta:nextLCMDs) (TD rightTimeDelta:nextRCMDs) =
   case ( compare leftTimeDelta rightTimeDelta ) of
      LT -> TD leftTimeDelta:( nextLCMDs `mePar` ( TD ( rightTimeDelta - leftTimeDelta ) : nextRCMDs ) )
      EQ -> TD leftTimeDelta:( nextLCMDs `mePar` nextRCMDs )
      GT -> ( TD rightTimeDelta:nextRCMDs ) `mePar` ( TD leftTimeDelta:nextLCMDs)

-- Converts musical events to midi events
mEventsToMidiEvents :: InstrumentMap -> [MEvent IEvent] -> [MIDI.MidiCmd]
mEventsToMidiEvents (InstrumentMap iArray) (mevent:nextMEvents) =
   case mevent of
      IE (IEvent (InstrumentId iIndex) ievent ) ->
         case ( fromDynamic $ iArray ! iIndex ) of
            Just instrument ->
               let (newInstrument, midiChannelMsg) = instrumentEventToMIDI instrument ievent
                   newIArray = iArray // [(iIndex, toDyn newInstrument)]
               in MIDI.ChannelMsgCmd midiChannelMsg:(mEventsToMidiEvents (InstrumentMap newIArray) nextMEvents)
            Nothing -> error "Instrument's event type mismatch"
      TD timeDelta ->
         (MIDI.TimestampDelta $ floor $ timeDelta):(mEventsToMidiEvents (InstrumentMap iArray) nextMEvents)
