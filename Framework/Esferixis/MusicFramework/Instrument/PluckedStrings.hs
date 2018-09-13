{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Instrument.PluckedStrings(
     PluckedStrCmd(StrPick, StrDamp, StrPitch)
   ) where

import Data.Word
import Data.Maybe

import Esferixis.MusicFramework.Instrument

data PluckedStrCmd a h =
   StrPick Double a | -- Tocado de la cuerda, con la posición y la amplitud especificados
   StrDamp Double | -- Damping de la cuerda
   StrPitch h -- Longitud efectiva de la cuerda (Altura)
