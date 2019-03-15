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
   GEvent(GStringPick, GStringMute, GPalmMutting, GModWheel)
   ) where

import qualified Esferixis.MusicFramework.Music as M
import qualified Data.Sequence as S
import qualified Data.List as L
import Data.Maybe

data GuitarTuning = GuitarTuning (Int -> Int)

mkGuitarTuning :: (Int, Int, Int, Int, Int, Int) -> GuitarTuning
mkGuitarTuning (t1, t2, t3, t4, t5, t6) =
   let rawTuning = S.fromList [t1, t2, t3, t4, t5, t6]
   in GuitarTuning $ \stringNumber -> fromJust $ S.lookup stringNumber rawTuning

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

gStringTuning :: GuitarTuning -> Int -> Int
gStringTuning (GuitarTuning fun) = fun

data GEvent =
   GStringPick GString Int Double |
   GStringMute GString |
   GPalmMutting Bool |
   GModWheel Double

instance M.Instrument GEvent where
   transposeIEvent (GStringPick string pitch velocity) pitchDelta = GStringPick string (pitch+pitchDelta) velocity
   transposeIEvent event pitchDelta = event
   modWheel = GModWheel
