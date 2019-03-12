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

data MidiCmd =
   ChannelMsg Int MidiMsg | -- Channel number, message
   TimestampDelta Int -- Timestamp delta in microseconds

-- MIDI paralellization
midiPar :: [MidiCmd] -> [MidiCmd] -> [MidiCmd]
midiPar cmds [] = cmds
midiPar [] cmds = cmds
midiPar (TimestampDelta leftTimeDelta:nextLCMDs) (TimestampDelta rightTimeDelta:nextRCMDs) =
   case ( compare leftTimeDelta rightTimeDelta ) of
      LT -> TimestampDelta leftTimeDelta:( nextLCMDs `midiPar` ( TimestampDelta ( rightTimeDelta - leftTimeDelta ) : nextRCMDs ) )
      EQ -> TimestampDelta leftTimeDelta:( nextLCMDs `midiPar` nextRCMDs )
      GT -> (TimestampDelta rightTimeDelta:nextRCMDs) `midiPar` (TimestampDelta leftTimeDelta:nextLCMDs)
