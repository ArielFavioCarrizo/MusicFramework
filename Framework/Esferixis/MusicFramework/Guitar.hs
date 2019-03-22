-- |
-- Module      :  Esferixis.MusicFramework.Guitar
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

module Esferixis.MusicFramework.Guitar(
   GuitarTuning,
   mkGuitarTuning,
   standardGuitarTuning,
   GString,
   gStringNumber,
   gs1,
   gs2,
   gs3,
   gs4,
   gs5,
   gs6,
   numberOfGuitarStrings,
   gStringTuning,
   GEvent(GStringPick, GStringMute, GMuteAll, GPalmMutting, GPitchWheel, GModWheel),
   gsPick, gsMute, gPalmMutting, gPitchWheel, gModWheel, gMuteAll
   ) where

import qualified Esferixis.MusicFramework.Music as M
import qualified Data.Sequence as S
import qualified Data.List as L
import Data.Maybe

data GuitarTuning = GuitarTuning (Int -> M.Pitch)

mkGuitarTuning :: (M.Pitch, M.Pitch, M.Pitch, M.Pitch, M.Pitch, M.Pitch) -> GuitarTuning
mkGuitarTuning (t6, t5, t4, t3, t2, t1) =
   let rawTuning = S.fromList [t1, t2, t3, t4, t5, t6]
   in GuitarTuning $ \stringNumber -> fromJust $ S.lookup stringNumber rawTuning

standardGuitarTuning :: GuitarTuning = mkGuitarTuning (M.e 2, M.a 2, M.d 3, M.g 3, M.b 3, M.e 4)

data GString = GString Int

gStringNumber :: GString -> Int
gStringNumber (GString number) = number

gs1 = GString 0
gs2 = GString 1
gs3 = GString 2
gs4 = GString 3
gs5 = GString 4
gs6 = GString 5

numberOfGuitarStrings = 6 :: Int

gStringTuning :: GuitarTuning -> Int -> M.Pitch
gStringTuning (GuitarTuning fun) = fun

data GEvent =
   GStringPick GString Int Double |
   GStringMute GString |
   GMuteAll |
   GPalmMutting Bool |
   GPitchWheel Double |
   GModWheel Double

instance M.Instrument GEvent where
   transposeIEvent pitchDelta (GStringPick string pitch velocity) = GStringPick string (pitch+pitchDelta) velocity
   transposeIEvent pitchDelta event = event
   velocityIEvent vFactor (GStringPick string pitch velocity) = GStringPick string pitch (velocity * vFactor)
   velocityIEvent vFactor event = event
   modWheel = GModWheel

gsPick = GStringPick
gsMute = GStringMute
gPalmMutting = GPalmMutting
gPitchWheel = GPitchWheel
gModWheel = GModWheel
gMuteAll = GMuteAll
