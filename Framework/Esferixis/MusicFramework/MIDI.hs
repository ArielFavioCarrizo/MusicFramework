-- |
-- Module      :  Esferixis.MusicFramework.MIDI
-- Copyright   :  (c) 2019 Ariel Favio Carrizo
-- License     :  BSD-3-Clause
-- Stability   : experimental
-- Portability : ghc

module Esferixis.MusicFramework.MIDI where

data MidiMsg =
   NoteOff Int Int | -- Key, velocity
   NoteOn Int Int | -- Key, velocity
   PolyAftertouch Int Int | -- Polyphonic key pressure (key, pressure)
   CC Int Int | -- Control change (Controller id, value)
   ProgramChange Int | -- Program change
   Aftertouch Int | -- Global aftertouch
   PitchWheel Int -- From -8192 to 8191

data ChannelMsg = ChannelMsg Int MidiMsg -- Channel number, message

data MidiCmd =
   ChannelMsgCmd ChannelMsg | -- Midi message for channel
   TimestampDelta Int -- Timestamp delta in microseconds
