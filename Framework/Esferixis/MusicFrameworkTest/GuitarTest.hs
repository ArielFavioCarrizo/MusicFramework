-- |
-- Module      :  Esferixis.MusicFrameworkTest.Guitar
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

module Esferixis.MusicFrameworkTest.GuitarTest where

import Esferixis.MusicFramework.Guitar
import qualified Esferixis.MusicFramework.KeyboardGuitar as KG
import Esferixis.MusicFramework.Music
import Esferixis.MusicFramework.MIDIMusic as MIDIMusic
import Esferixis.MusicFramework.HMidiBackend

revitarGuitarCfg :: KG.GuitarCfg = 
   KG.GuitarCfg {
      KG.gCfgChannelNumber = 1,
      KG.gCfgPalmMuting = MIDIMusic.MidiBoolFlagCfgByCC 15,
      KG.gCfgKeyMapping = MIDIMusic.IdMidiKeyMapping,
      KG.gCfgModWheelCC = 1,
      KG.gCfgTuning = standardGuitarTuning
      }
      
lethalityGuitarCfg :: KG.GuitarCfg =
   KG.GuitarCfg {
      KG.gCfgChannelNumber = 2,
      KG.gCfgPalmMuting = MIDIMusic.MidiBoolFlagCfgByKeys (absPitch 60) (absPitch 61),
      KG.gCfgKeyMapping =
         MIDIMusic.AffineMidiKeyMapping $
            MIDIMusic.AffineMidiKeyMappingDesc {
               midiKeyMappingMinInPitch = e 2,
               midiKeyMappingMinOutPitch = absPitch 64,
               midiKeyMappingMaxOutPitch = absPitch 103
            },
      KG.gCfgModWheelCC = 1,
      KG.gCfgTuning = standardGuitarTuning
   }
   
guitarCfg = revitarGuitarCfg

test2 :: MContext c (Music c)
test2 = do
   guitar <- mkInstrument $ KG.mkGuitarSt guitarCfg

   return $
      (guitar $ gPalmMutting True) :+:
      (guitar $ gsPick gs6 0 1.0) :+: unitTD :+:
      (guitar $ gMuteAll)


test1 :: MContext c (Music c)
test1 = do
   guitar <- mkInstrument $ KG.mkGuitarSt guitarCfg
   
   return $
      let
         pm value = guitar $ gPalmMutting value
         gStop = guitar gMuteAll
         pc7 h = transpose h $ ((guitar $ gsPick gs6 0 1.0) :=: (guitar $ gsPick gs5 2 1.0)) :+: unitTD :+: gStop
         pc5 h = transpose h $ ((guitar $ gsPick gs6 0 1.0) :=: (guitar $ gsPick gs5 0 1.0)) :+: unitTD :+: gStop
         x1 = (guitar $ gsPick gs6 0 1.0) :+: unitTD :+: gStop
         
         x2 = (pm True) :+: x1 :+: ( r 2 $ tempo (1/2) x1 ) :+: x1
         x3 = (pm True) :+: ( r 2 $ tempo (1/2) x1 )
         
         x5 = (r 3 x2) :+: x3
         x6 = (pm False) :+: (transpose 3 $ line $ map pc7 [0, 1, 2, 1])
         --x6 = (pm False) :+: (transpose 3 $ (line $ map pc7 [0, 1, 2]) :+: (pc5 0))
         x7 = (r 2 x5) :+: x6
         
      in tempo (1/6) $ (pm True) :+: (infLoop x7)
