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
import Data.Sequence
import Data.Maybe
import qualified Data.List as L

data GConfig = GConfig {
   nChannel :: Int,
   tuningByString :: Seq Double
   }

data GString = GString Int

data GEvent =
   GStringPick GString Double |
   GPalmMutting Bool

data GuitarSt = GuitarSt {
   guitarStStringSt :: Seq StringSt,
   guitarStGConfig :: GConfig
   }

data StringSt = StringSt {
   stringStLastNotePitch :: Maybe Int
   }

