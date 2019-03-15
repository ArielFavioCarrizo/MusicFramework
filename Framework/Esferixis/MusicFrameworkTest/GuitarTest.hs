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
import Esferixis.MusicFramework.HMidiBackend

guitarCfg :: KG.GuitarCfg = 
   KG.GuitarCfg {
      KG.gCfgChannelNumber = 0,
      KG.gCfgPalmMutingCC = 15,
      KG.gCfgModWheelCC = 1,
      KG.gCfgTuning = standardGuitarTuning
      }

test2 :: MContext c (Music c)
test2 = do
   guitar <- mkInstrument $ KG.mkGuitarSt guitarCfg

   return $
      (guitar $ gsPick gs6 0 1.0) :+:
      (guitar $ gMuteAll)

test1 :: MContext c (Music c)
test1 = do
   guitar <- mkInstrument $ KG.mkGuitarSt guitarCfg
   
   return $
      let pm = \value -> guitar $ gPalmMutting value
          pc7 = (guitar $ gsPick gs6 0 1.0) :=: (guitar $ gsPick gs5 2 1.0)
          pc5 = (guitar $ gsPick gs6 0 1.0) :=: (guitar $ gsPick gs5 0 1.0)
          gStop = guitar gMuteAll
          x1 = (guitar $ gsPick gs6 0 1.0) :+: unitTD
          x2 = r 12 x1
          x3 = tempo (4/9) (r 3 x1)
          x4 = x1
          x5 = x3 :+: x4 :+: x3
          x6 = x2 :+: ( r 1 x5 )
          x7 = r 3 x6
      in
          tempo (1/6) $
             (pm True) :+: x7
