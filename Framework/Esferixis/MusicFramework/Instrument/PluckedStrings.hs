{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Instrument.PluckedStrings(
     PluckedStrCmd(StrPick, StrDamp, StrPitch)
   ) where

import Data.Word
import Data.Maybe

import Esferixis.MusicFramework.Notation

data PluckedStrCmd a h = StrPick a | StrDamp Double | StrPitch h 

data PluckedStrPlay t a h = PluckedStrPlay [[IEvent t (PluckedStrCmd a h)]]
