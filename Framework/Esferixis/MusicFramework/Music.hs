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
{-# LANGUAGE InstanceSigs #-}

module Esferixis.MusicFramework.Music(
   Instrument(transposeIEvent),
   InstrumentState(InstrumentState, instrumentEventToMIDI),
   InstrumentId,
   IEvent(IEvent),
   MEvent(IE, TD),
   Music(ME, (:+:), (:=:)),
   mMap,
   transpose,
   tdelta,
   MContext,
   mkInstrument,
   toMidi
   ) where

import qualified Esferixis.MusicFramework.MIDI as MIDI
import Data.Proxy
import Data.Array
import Data.Dynamic
import Data.Maybe
import qualified Data.List as L
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

class (Typeable e) => Instrument e where
   transposeIEvent :: e -> Double -> e -- Transpose pitch

{-
   Instrument state

   e: Instrument's event
-}
data InstrumentState e = InstrumentState {
   instrumentEventToMIDI :: (Instrument e) => e -> (InstrumentState e, MIDI.MidiCmd)
   }

data InstrumentId c e = InstrumentId Int

data IEvent c where
   IEvent :: (Instrument e) => InstrumentId c e -> e -> IEvent c

-- Musical event
data MEvent e =
   IE e | -- Instrument event
   TD Double -- Time delta

-- Music DSL
data Music c =
   ME (MEvent (IEvent c)) | -- Instrument event
   Music c :+: Music c | -- Sequential composition
   Music c :=: Music c -- Parallel composition

-- Map music events
mMap :: (MEvent (IEvent c) -> MEvent (IEvent c)) -> Music c -> Music c
mMap mFun music =
   let self = mMap mFun
   in
      case music of
         ME mEvent -> ME $ mFun mEvent
         leftMusic :+: rightMusic -> ( self leftMusic ) :+: ( self rightMusic )
         leftMusic :=: rightMusic -> ( self leftMusic ) :=: ( self rightMusic )

-- Transpose music's pitch
transpose :: Music c -> Double -> Music c
transpose music pitchDelta =
   let mFun mEvent =
          case mEvent of
             IE (IEvent instrId iEvent) -> IE $ IEvent instrId $ transposeIEvent iEvent pitchDelta
             _ -> mEvent

   in mMap mFun music

-- Makes a music value that represents a timestamp delta from raw timestamp delta value
tdelta :: Double -> Music c
tdelta timeDelta =
   ME $ TD timeDelta

-- Musical context monad
data MContext c r where
   MContextMkInstrument :: (Instrument e) => InstrumentState e -> MContext c ( e -> Music r )
   MContextBind :: MContext c s -> ( s -> MContext c r ) -> MContext c r
   MContextReturn :: r -> MContext c r
   MContextFail :: String -> MContext c r

instance Monad (MContext c) where
   (>>=) :: forall s r. MContext c s -> ( s -> MContext c r ) -> MContext c r
   preMContext >>= k = MContextBind preMContext k
   
   return value = MContextReturn value

   fail message = MContextFail message

instance Applicative (MContext c) where
   pure = return
   (<*>) = ap

instance Functor (MContext c) where
   fmap = liftM

mkInstrument :: (Instrument e) => InstrumentState e -> MContext c ( e -> Music c )
mkInstrument = MContextMkInstrument

toMidi :: (forall c. MContext c (Music c)) -> [MIDI.MidiEvent]
toMidi mContext =
   let (rawInstrMap, music) = runMContext [] mContext
   in
      case (L.uncons rawInstrMap) of
         Just ( (lastIndex, _), _ ) ->
            mEventsToMidiEvents (InstrumentMap $ array (0, lastIndex) rawInstrMap) (musicToEvents music)
         Nothing ->
            []

-- Instrument map
data InstrumentMap = InstrumentMap (Array Int Dynamic)

runMContext :: [(Int, Dynamic)] -> MContext c r -> ([(Int, Dynamic)], r)
runMContext instrEntries mContext =
   case mContext of
      MContextMkInstrument instrState ->
          let newIndex =
                 case (L.uncons instrEntries) of
                    Just ( (lastIndex, _), _ ) -> lastIndex + 1
                    Nothing -> 0
              putInstrEventFun = \iEvent -> ME $ IE $ IEvent (InstrumentId newIndex) iEvent
          in ( (newIndex, toDyn instrState):instrEntries, putInstrEventFun )
      MContextBind preMContext transformMContextFun ->
          let (newInstrEntries, preValue) = runMContext instrEntries preMContext
          in runMContext newInstrEntries $ transformMContextFun preValue
      MContextReturn value ->
          (instrEntries, value)
      MContextFail message ->
          error message

-- Converts music to a list of events
musicToEvents :: Music c -> [MEvent (IEvent c)]
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
mEventsToMidiEvents :: InstrumentMap -> [MEvent (IEvent c)] -> [MIDI.MidiEvent]
mEventsToMidiEvents (InstrumentMap iArray) (mevent:nextMEvents) =
   case mevent of
      IE (IEvent (InstrumentId iIndex) ievent ) ->
         case ( fromDynamic $ iArray ! iIndex ) of
            Just instrument ->
               let (newInstrument, midiCmd) = instrumentEventToMIDI instrument ievent
                   newIArray = iArray // [(iIndex, toDyn newInstrument)]
               in MIDI.MEvCmd midiCmd:(mEventsToMidiEvents (InstrumentMap newIArray) nextMEvents)
            Nothing -> error "Instrument's event type mismatch"
      TD timeDelta ->
         (MIDI.MEvTimestampDelta $ floor $ timeDelta):(mEventsToMidiEvents (InstrumentMap iArray) nextMEvents)
