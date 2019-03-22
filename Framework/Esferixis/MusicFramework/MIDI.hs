-- |
-- Module      :  Esferixis.MusicFramework.MIDI
-- Copyright   :  (c) 2019 Ariel Favio Carrizo
-- License     :  BSD-3-Clause
-- Stability   : experimental
-- Portability : ghc

module Esferixis.MusicFramework.MIDI where

import Text.Show

data MidiMsg =
   NoteOff Int Int | -- Key, velocity
   NoteOn Int Int | -- Key, velocity
   PolyAftertouch Int Int | -- Polyphonic key pressure (key, pressure)
   CC Int Int | -- Control change (Controller id, value)
   ProgramChange Int | -- Program change
   Aftertouch Int | -- Global aftertouch
   PitchWheel Int -- From -8192 to 8191

data MidiCmd =
   ChannelMsg Int MidiMsg -- Channel number, message

data MidiEvent =
   MEvCmd MidiCmd | -- Midi message
   MEvTimestampDelta Int -- Timestamp delta in microseconds

instance Show MidiEvent where
   show (MEvCmd midiCmd) = "MEvCmd (" ++ (show midiCmd) ++ ")"
   show (MEvTimestampDelta value) = "MEvTimestampDelta " ++ show value

instance Show MidiCmd where
   show (ChannelMsg channelNumber midiMsg) = "ChannelMsg " ++ (show channelNumber) ++ "(" ++ (show midiMsg) ++ ")"

instance Show MidiMsg where
   show (NoteOff key velocity) = "NoteOff " ++ (show key) ++ " " ++ (show velocity)
   show (NoteOn key velocity) = "NoteOn " ++ (show key) ++ " " ++ (show velocity)
   show (PolyAftertouch key pressure) = "PolyAftertouch " ++ (show key) ++ " " ++ (show pressure)
   show (CC controllerId value) = "CC " ++ (show controllerId) ++ " " ++ (show value)
   show (ProgramChange value) = "ProgramChange " ++ (show value)
   show (Aftertouch value) = "Aftertouch " ++ (show value)
   show (PitchWheel value) = "PitchWheel " ++ (show value)
