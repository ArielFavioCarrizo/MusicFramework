{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Instrument(
     DInstrEvent(DInstrEvent)
   , DInstrPlaying(DInstrPlaying)
   ) where

import Data.Word
import Data.Maybe

-- Evento de instrumento discreto, donde lleva el tiempo relativo de accionamiento
data DInstrEvent t c = DInstrEvent t c

data DInstrPlaying t c = DInstrPlaying [DInstrEvent t c] -- Tocado de instrumento discreto
