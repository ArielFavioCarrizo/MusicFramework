{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Notation(
   IEvent(IEvent)
   ) where

import Data.Word
import Data.Maybe

-- Evento de instrumento
data IEvent t c = IEvent t c
