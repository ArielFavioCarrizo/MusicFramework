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
   Instrument(transposeIEvent, velocityIEvent, modWheel),
   InstrumentState(InstrumentState, instrumentEventToMIDI),
   InstrumentId,
   PInstrument,
   Pitch,
   cf, c, cs, df, d, ds, ef, e, es, ff, f, fs, gf, g, gs, af, a, as, bf, b, bs,
   octaveOffset,
   absPitch,
   pitchValue,
   absPitchMap,
   IEvent(IEvent),
   MEvent(IE, TD),
   Music(ME, MEmpty, (:+:), (:=:)),
   mMap,
   transpose,
   tempo,
   td,
   unitTD,
   iline,
   line,
   r,
   infLoop,
   ra,
   MContext,
   mkInstrument,
   toMidi
   ) where

import qualified Esferixis.MusicFramework.MIDI as MIDI
import Data.Proxy
import qualified Data.Sequence as S
import Data.Dynamic
import Data.Maybe
import qualified Data.List as L
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

class (Typeable e) => Instrument e where
   transposeIEvent :: Int -> e -> e -- Transpose pitch
   velocityIEvent :: Double -> e -> e -- Adjust velocity
   modWheel :: Double -> e -- Modwheel event

{-
   Instrument state

   e: Instrument's event
-}
data InstrumentState e = InstrumentState {
   instrumentEventToMIDI :: (Instrument e) => e -> (Maybe (InstrumentState e), [MIDI.MidiCmd])
   }

data InstrumentId c e = InstrumentId Int

type PInstrument c e = e -> Music c

data Pitch = Pitch Int

cf :: Int -> Pitch
cf = absPitchV (-1)

c :: Int -> Pitch
c = absPitchV 0

cs :: Int -> Pitch
cs = absPitchV 1

df :: Int -> Pitch
df = absPitchV 1

d :: Int -> Pitch
d = absPitchV 2

ds :: Int -> Pitch
ds = absPitchV 3

ef :: Int -> Pitch
ef = absPitchV 3

e :: Int -> Pitch
e = absPitchV 4

es :: Int -> Pitch
es = absPitchV 5

ff :: Int -> Pitch
ff = absPitchV 4

f :: Int -> Pitch
f = absPitchV 5

fs :: Int -> Pitch
fs = absPitchV 6

gf :: Int -> Pitch
gf = absPitchV 6

g :: Int -> Pitch
g = absPitchV 7

gs :: Int -> Pitch
gs = absPitchV 8

af :: Int -> Pitch
af = absPitchV 8

a :: Int -> Pitch
a = absPitchV 9

as :: Int -> Pitch
as = absPitchV 10

bf :: Int -> Pitch
bf = absPitchV 10

b :: Int -> Pitch
b = absPitchV 11

bs :: Int -> Pitch
bs = absPitchV 12

absPitch :: Int -> Pitch
absPitch value = Pitch value

pitchValue :: Pitch -> Int
pitchValue (Pitch value) = value

absPitchMap :: (Int -> Int) -> Pitch -> Pitch
absPitchMap fun (Pitch value) = Pitch $ fun value

octaveOffset :: Int -> Pitch
octaveOffset octaveNumber = Pitch $ 12 * (octaveNumber + 1)

absPitchV :: Int -> Int -> Pitch
absPitchV relativePitchValue octaveNumber = absPitchMap (+ relativePitchValue) (octaveOffset octaveNumber)

data IEvent c where
   IEvent :: (Instrument e) => InstrumentId c e -> e -> IEvent c

-- Musical event
data MEvent e =
   IE e | -- Instrument event
   TD Double -- Time delta

-- Music DSL
data Music c =
   ME (MEvent (IEvent c)) | -- Instrument event
   MEmpty |
   Music c :+: Music c | -- Sequential composition
   Music c :=: Music c -- Parallel composition

-- Map music events
mMap :: (MEvent (IEvent c) -> MEvent (IEvent c)) -> Music c -> Music c
mMap mFun music =
   let self = mMap mFun
   in
      case music of
         ME mEvent -> ME $ mFun mEvent
         MEmpty -> MEmpty
         leftMusic :+: rightMusic -> ( self leftMusic ) :+: ( self rightMusic )
         leftMusic :=: rightMusic -> ( self leftMusic ) :=: ( self rightMusic )

-- Transpose music's pitch
transpose :: Int -> Music c -> Music c
transpose pitchDelta music =
   let mFun mEvent =
          case mEvent of
             IE (IEvent instrId iEvent) -> IE $ IEvent instrId $ transposeIEvent pitchDelta iEvent
             _ -> mEvent

   in mMap mFun music

-- Adjust velocity
velocity :: Double -> Music c -> Music c
velocity vFactor music =
   let mFun mEvent =
          case mEvent of
             IE (IEvent instrId iEvent) -> IE $ IEvent instrId $ velocityIEvent vFactor iEvent
             _ -> mEvent

   in mMap mFun music

-- Modify tempo
tempo :: Double -> Music c -> Music c
tempo tfactor music =
   let mFun mEvent =
          case mEvent of
             TD timeDelta -> TD $ timeDelta * tfactor
             _ -> mEvent
   in mMap mFun music

-- Makes a music value that represents a timestamp delta from raw timestamp delta value
td :: Double -> Music c
td timeDelta =
   ME $ TD timeDelta

-- Unit time delta
unitTD :: Music c
unitTD = ME $ TD 1

-- Instrument events
iline :: PInstrument c e -> [e] -> Music c
iline instrument [] = MEmpty
iline instrument (eachIEvent:nextIEvents) = (instrument eachIEvent) :+: (iline instrument nextIEvents)

-- Music events
line :: [Music c] -> Music c
line [] = MEmpty
line (head:tail) = head :+: (line tail)

-- Repeats the given music value with the given times
r :: Int -> Music c -> Music c
r times music
   | times > 0 = music :+: ( r (times - 1) music ) 
   | otherwise = MEmpty

infLoop :: Music c -> Music c
infLoop music = music :+: (infLoop music)

ra :: Int -> Music c -> Music c -> Music c
ra times musicA musicB = (r times (musicA :+: musicB)) :+: musicB

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
   let (instrMap, music) = runMContext S.empty mContext
   in mEventsToMidiEvents instrMap (musicToEvents music)

runMContext :: S.Seq Dynamic -> MContext c r -> (S.Seq Dynamic, r)
runMContext instrMap mContext =
   case mContext of
      MContextMkInstrument instrState ->
          let newIndex = S.length instrMap
              putInstrEventFun = \iEvent -> ME $ IE $ IEvent (InstrumentId newIndex) iEvent
          in ( instrMap S.|> (toDyn instrState), putInstrEventFun )
      MContextBind preMContext transformMContextFun ->
          let (newInstrMap, preValue) = runMContext instrMap preMContext
          in runMContext newInstrMap $ transformMContextFun preValue
      MContextReturn value ->
          (instrMap, value)
      MContextFail message ->
          error message

-- Converts music to a list of events
musicToEvents :: Music c -> [MEvent (IEvent c)]
musicToEvents (ME mevent) =
   [mevent]
musicToEvents MEmpty = []
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
mePar (TD leftTimeDelta:nextLCMDs) (rightCMD:nextRightCMDs) =
   rightCMD:( mePar (TD leftTimeDelta:nextLCMDs) nextRightCMDs )
mePar leftCMDs (TD rightTimeDelta:nextRCMDs) =
   mePar (TD rightTimeDelta:nextRCMDs) leftCMDs
mePar (leftCMD:nextLCMDs) (rightCMD:nextRCMDs) =
   [leftCMD, rightCMD] ++ (mePar nextLCMDs nextRCMDs)

-- Converts musical events to midi events
mEventsToMidiEvents :: S.Seq Dynamic -> [MEvent (IEvent c)] -> [MIDI.MidiEvent]
mEventsToMidiEvents instrMap [] = []
mEventsToMidiEvents instrMap (mevent:nextMEvents) =
   case mevent of
      IE (IEvent (InstrumentId iIndex) ievent ) ->
         case ( S.lookup iIndex instrMap ) of
            Just instrSt ->
               case (fromDynamic instrSt) of
                  Just instrument ->
                     let (maybeNewInstrSt, midiCMDs) = instrumentEventToMIDI instrument ievent
                         newInstrMap =
                            case maybeNewInstrSt of
                               Just newInstrSt -> S.update iIndex (toDyn newInstrSt) instrMap
                               Nothing -> instrMap
                     in (map MIDI.MEvCmd midiCMDs) ++ (mEventsToMidiEvents newInstrMap nextMEvents)
                  Nothing -> error "Instrument's event type mismatch"
            Nothing -> error "Instrument id doesn't exists"
      TD timeDelta ->
         (MIDI.MEvTimestampDelta $ floor $ (timeDelta * 1000000) ):(mEventsToMidiEvents instrMap nextMEvents)
