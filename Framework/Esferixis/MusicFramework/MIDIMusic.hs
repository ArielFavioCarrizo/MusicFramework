-- |
-- Module      :  Esferixis.MusicFramework.MIDIMusic
-- Copyright   :  (c) 2019 Ariel Favio Carrizo
-- License     :  BSD-3-Clause
-- Stability   : experimental
-- Portability : ghc

module Esferixis.MusicFramework.MIDIMusic(
   MidiBoolFlagCfg(MidiBoolFlagCfgByCC, MidiBoolFlagCfgByKeys),
   MidiBoolFlagSt,
   mkMidiBoolFlag,
   setMidiBoolFlag,
   MidiKeyMapping(IdMidiKeyMapping, AffineMidiKeyMapping),
   AffineMidiKeyMappingDesc(AffineMidiKeyMappingDesc, midiKeyMappingMinInPitch, midiKeyMappingMinOutPitch, midiKeyMappingMaxOutPitch),
   mkMidiKeyMappingFun
   ) where

import Esferixis.MusicFramework.MIDI as MIDI
import Esferixis.MusicFramework.Music as M
import Data.Bool
import Control.Monad.State.Lazy

data MidiBoolFlagCfg =
   MidiBoolFlagCfgByCC Int | -- Boolean flag based on CC
   MidiBoolFlagCfgByKeys M.Pitch M.Pitch -- Boolean flag based on keys

data MidiBoolFlagSt = MidiBoolFlagSt (Bool -> (MidiBoolFlagSt, [MidiMsg]))

mkMidiBoolFlag :: MidiBoolFlagCfg -> MidiBoolFlagSt
mkMidiBoolFlag flagCfg =
   case flagCfg of
      MidiBoolFlagCfgByCC cId -> MidiBoolFlagSt $ \value -> (mkMidiBoolFlag flagCfg, [MIDI.CC cId $ bool 0 127 value])
      MidiBoolFlagCfgByKeys lowKey highKey ->
         let mkFlagSt oldValue =
                MidiBoolFlagSt $ \newValue ->
                   let lowKeyValue = pitchValue lowKey
                       highKeyValue = pitchValue highKey
                       stKeyValue = bool lowKeyValue highKeyValue
                       outMIDICMDs =
                          if ( newValue /= oldValue )
                             then [MIDI.NoteOff (stKeyValue oldValue) 127, MIDI.NoteOn (stKeyValue newValue) 127]
                             else []
                   in (mkFlagSt newValue, outMIDICMDs)
         in mkFlagSt False
         
setMidiBoolFlag :: MidiBoolFlagSt -> Bool -> (MidiBoolFlagSt, [MidiMsg])
setMidiBoolFlag (MidiBoolFlagSt fun) = fun
      
data MidiKeyMapping =
   IdMidiKeyMapping |
   AffineMidiKeyMapping AffineMidiKeyMappingDesc
   
data AffineMidiKeyMappingDesc =
   AffineMidiKeyMappingDesc {
      midiKeyMappingMinInPitch :: Pitch,
      midiKeyMappingMinOutPitch :: Pitch,
      midiKeyMappingMaxOutPitch :: Pitch
      }
      
mkMidiKeyMappingFun :: MidiKeyMapping -> (Pitch -> Int)
mkMidiKeyMappingFun IdMidiKeyMapping = M.pitchValue
mkMidiKeyMappingFun (AffineMidiKeyMapping midiKeyMapping) =
   let minInPitchValue = M.pitchValue $ midiKeyMappingMinInPitch midiKeyMapping
       minOutPitchValue = M.pitchValue $ midiKeyMappingMinOutPitch midiKeyMapping
       maxOutPitchValue = M.pitchValue $ midiKeyMappingMaxOutPitch midiKeyMapping
       maxInPitchValue = (maxOutPitchValue - minOutPitchValue) + minInPitchValue
   in
      if ( maxOutPitchValue >= minOutPitchValue )
         then
            \inPitch ->
               let inPitchValue = M.pitchValue inPitch
               in
                  if ( inPitchValue >= minInPitchValue )
                     then
                        if ( inPitchValue <= maxInPitchValue )
                           then
                              inPitchValue + minOutPitchValue - minInPitchValue
                           else
                              error "Pitch overflow"
                     else
                        error "Pitch underflow"
         else
            error "MaxOutPitch < MinOutPitch!"
