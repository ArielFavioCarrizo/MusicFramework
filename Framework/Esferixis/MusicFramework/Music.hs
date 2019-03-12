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

module Esferixis.MusicFramework.Music where

import qualified Esferixis.MusicFramework.MIDI as MIDI

{-
   Instrument

   i: Instrument
   e: Instrument's event
-}
class Instrument i e where
   instrumentEventToMIDI :: i -> e -> (Int, MIDI.MidiMsg) -- Transforms the given instrument's event into midi channel number and midi message

data Music where
   IEvent :: (Instrument i e) => i -> e -> Music -- Instrument event
   TDelta :: Double -> Music -- Time delta
   MEmpty :: Music -- Empty music
   (:+:) :: Music -> Music -> Music -- Sequential composition
   (:=:) :: Music -> Music -> Music -- Parallel composition

-- Converts music to midi
musicToMidi :: Music -> [MIDI.MidiCmd]
musicToMidi (IEvent instrument instrumentEvent) =
   let (nChannel, midiMsg) = instrumentEventToMIDI instrument instrumentEvent
   in [MIDI.ChannelMsg nChannel midiMsg]
musicToMidi (TDelta delta) =
   [MIDI.TimestampDelta $ floor $ delta]
musicToMidi MEmpty =
   []
musicToMidi ( leftMusic :+: rightMusic ) =
   ( musicToMidi leftMusic ) ++ ( musicToMidi rightMusic )
musicToMidi ( leftMusic :=: rightMusic ) =
   ( musicToMidi leftMusic ) `midiPar` ( musicToMidi rightMusic )
