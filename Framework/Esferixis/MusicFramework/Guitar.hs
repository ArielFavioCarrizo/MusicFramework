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

module Esferixis.MusicFramework.Guitar where

import qualified Esferixis.MusicFramework.MIDI as MIDI
import qualified Esferixis.MusicFramework.Music as M
import Data.Proxy
import Data.Array
import Data.Dynamic
import Data.Maybe
import qualified Data.List as L

data GuitarConfig = GuitarConfig {
   nChannel :: Int,
   tuning :: Array Int Double
   }

data GEvent =
   GStringPick Int Double |
   GPalmMutting Bool

data GuitarSt = GuitarData {
   stringSt :: Array Int StringSt,
   guitarCfg :: GuitarConfig
   }
   
data StringSt = StringSt {
   lastPitch :: Maybe Int
   }

{-
initialGuitarSt :: M.InstrumentState GEvent
initialGuitarSt =
   GuitarData {
   stringSt = array (0, 5) StringSt { lastPitch = Nothing }
   }
-}
