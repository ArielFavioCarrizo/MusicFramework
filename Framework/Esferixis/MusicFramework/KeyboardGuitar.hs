-- |
-- Module      :  Esferixis.MusicFramework.KeyboardGuitar
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

module Esferixis.MusicFramework.KeyboardGuitar(
   GuitarCfg(GuitarCfg, gCfgChannelNumber, gCfgPalmMutingCC, gCfgModWheelCC, gCfgTuning),
   mkGuitarSt
   ) where

import qualified Esferixis.MusicFramework.MIDI as MIDI
import qualified Esferixis.MusicFramework.Music as M
import qualified Esferixis.MusicFramework.Guitar as G
import qualified Data.Sequence as S
import Data.Maybe
import Data.Bool
import Data.Foldable
import qualified Data.List as L
import Control.Arrow

data GuitarCfg = GuitarCfg {
   gCfgChannelNumber :: Int,
   gCfgPalmMutingCC :: Int,
   gCfgModWheelCC :: Int,
   gCfgTuning :: G.GuitarTuning
   }

mkGuitarSt :: GuitarCfg -> M.InstrumentState G.GEvent
mkGuitarSt guitarCfg =
   mkGuitarInstrumentSt $
      GuitarSt {
         gStLastStringPitches = S.replicate G.numberOfGuitarStrings Nothing,
         gStGuitar =
            let mkMIDICmd = MIDI.ChannelMsg $ gCfgChannelNumber guitarCfg
            in
               Guitar {
                  gMkMIDICmd = MIDI.ChannelMsg $ gCfgChannelNumber guitarCfg,
                  gPalmMuting = (\value -> mkMIDICmd $ MIDI.CC (gCfgPalmMutingCC guitarCfg) $ bool 0 127 value),
                  gModWheel = (\value -> mkMIDICmd $ MIDI.CC (gCfgModWheelCC guitarCfg) $ floor $ (value + 1.0) * 127.0 / 2.0 ),
                  gTuning = gCfgTuning guitarCfg
                  }
         }


data Guitar = Guitar {
   gMkMIDICmd :: MIDI.MidiMsg -> MIDI.MidiCmd,
   gPalmMuting :: Bool -> MIDI.MidiCmd,
   gModWheel :: Double -> MIDI.MidiCmd,
   gTuning :: G.GuitarTuning
   }

data GuitarSt = GuitarSt {
   gStLastStringPitches :: S.Seq (Maybe Int),
   gStGuitar :: Guitar
   }

mkGuitarInstrumentSt :: GuitarSt -> M.InstrumentState G.GEvent
mkGuitarInstrumentSt guitarSt =
   M.InstrumentState {
      M.instrumentEventToMIDI = (gEventToMidi guitarSt) >>> (\(newGuitarSt, midiCMDs) -> (fmap mkGuitarInstrumentSt newGuitarSt, midiCMDs))
      }

gEventToMidi :: GuitarSt -> G.GEvent -> (Maybe GuitarSt, [MIDI.MidiCmd])
gEventToMidi guitarSt (G.GStringPick string rpitch velocity) =
   let guitar = gStGuitar guitarSt
       stringNumber = G.gStringNumber string
       lastStringPitches = gStLastStringPitches guitarSt
       Just maybeLastStringPitch = S.lookup stringNumber lastStringPitches
       pitchOffset = M.pitchValue $ G.gStringTuning (gTuning guitar) stringNumber
       absPitch = pitchOffset + rpitch
       noteOnMsg = [MIDI.NoteOn absPitch $ floor $ velocity * 127.0]
       isSamePitch = ( == Just absPitch )
       isSamePitchAsPrevious = isSamePitch maybeLastStringPitch
       noteOffMsg =
          case maybeLastStringPitch of
             Just lastStringPitch ->
                if ( ( not $ isSamePitchAsPrevious ) && (S.null $ S.filter isSamePitch $ S.deleteAt stringNumber lastStringPitches) )
                   then [MIDI.NoteOff lastStringPitch 127]
                   else []
             Nothing -> []
       nextGuitarSt = 
          if isSamePitchAsPrevious
             then Nothing
             else Just $ guitarSt { gStLastStringPitches = S.update stringNumber (Just absPitch) lastStringPitches }
   in (nextGuitarSt, map (gMkMIDICmd guitar) (noteOnMsg ++ noteOffMsg) )

gEventToMidi guitarSt (G.GStringMute string ) =
   let guitar = gStGuitar guitarSt
       stringNumber = G.gStringNumber string
       doNothing = (Nothing, [])
       lastStringPitches = gStLastStringPitches guitarSt
   in
      case ( S.lookup stringNumber lastStringPitches )  of
         Just (Just lastStringPitch) ->
            if (S.null $ S.filter ( == Just lastStringPitch ) $ S.deleteAt stringNumber lastStringPitches)
               then
                  let nextGuitarSt = guitarSt { gStLastStringPitches = S.update stringNumber Nothing lastStringPitches }
                      noteOff = [gMkMIDICmd guitar $ MIDI.NoteOff lastStringPitch 127]
                  in (Just $ nextGuitarSt, noteOff)
               else doNothing
         Nothing -> doNothing

gEventToMidi guitarSt G.GMuteAll =
    let lastStringPitches = toList $ gStLastStringPitches guitarSt
        noteOFFs = (map $ \(Just pitch) -> gMkMIDICmd (gStGuitar guitarSt) $ MIDI.NoteOff pitch 127 ) (filter isJust lastStringPitches)
    in ( Just $ guitarSt { gStLastStringPitches = S.replicate G.numberOfGuitarStrings Nothing }, noteOFFs)

gEventToMidi guitarSt (G.GPalmMutting value) = (Nothing, [gPalmMuting (gStGuitar guitarSt) value])

gEventToMidi guitarSt (G.GPitchWheel value) = (Nothing, [gMkMIDICmd (gStGuitar guitarSt) $ MIDI.PitchWheel ( ( floor $ (value + 1.0) * 16383 / 2.0 ) - 8192 ) ])

gEventToMidi guitarSt (G.GModWheel value) = (Nothing, [gModWheel (gStGuitar guitarSt) value])
