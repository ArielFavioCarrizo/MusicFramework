{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal.Stateful.Producer
   ( SFProducerSt() ) where

import Data.Word
import Data.Maybe

import Esferixis.MusicFramework.Signal.Stateful.SignalChunk

{- 
   Representación abstracta de un productor stateful
   no manejado
-}
data SFProducerSt m sc = UnmanagedProducerSt {
     sfpRemainingFrames :: Maybe Word64 -- Devuelve la cantidad de frames que quedan sin producir, si es infinito devuelve Nothing
   , sfpTick :: (Monad m, SFSignalChunk m sc) => SignalChunkSection sc -> m ( SFProducerSt m sc ) -- Realiza un 'tick', produce una sección de señal en la sección de chunk especificada y devuelve el próximo estado
   , sfpDelete :: (Monad m) => m () -- Destruye el productor
   }
