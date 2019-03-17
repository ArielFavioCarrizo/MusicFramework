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
      let
         pm value = guitar $ gPalmMutting value
         gStop = guitar gMuteAll
         pc7 h = transpose h $ ((guitar $ gsPick gs6 0 1.0) :=: (guitar $ gsPick gs5 2 1.0)) :+: unitTD :+: gStop
         pc5 h = transpose h $ ((guitar $ gsPick gs6 0 1.0) :=: (guitar $ gsPick gs5 0 1.0)) :+: unitTD :+: gStop
         x1 = (guitar $ gsPick gs6 0 1.0) :+: unitTD
         x10 = r 3 x1 :+: unitTD
         
         x7 =
            let
               x2 = r 12 x1
               x3 = tempo (4/9) $ transpose 1 $ r 3 x1
               x5 = x3 :+: x1 :+: x3
               x6 = x2 :+: x5
            in pm True :+: (r 3 x6) :+: gStop

         x8 = pm False :+: (tempo 2 $ (pc7 5) :+: (pc7 4))
         x9 = r 3 (x7 :+: x8)
         
      in tempo (1/6) $ (pm True) :+: x9
