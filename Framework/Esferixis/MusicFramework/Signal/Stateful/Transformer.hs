{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal.Stateful.Transformer
   ( SFTransformerSt() ) where

import Data.Word
import Data.Maybe

import Esferixis.MusicFramework.Signal.Stateful.SignalChunk

{- 
   Representación abstracta de un transformador stateful
   no manejado
-}
data SFTransformerSt m sc = UnmanagedProducerSt {
     sftRemainingFrames :: Maybe Word64 -- Devuelve la cantidad de frames que quedan sin transformar, si es infinito devuelve Nothing
   , sftTick :: (Monad m, SFSignalChunk m sc) => SignalChunkSection sc -> SignalChunkSection sc -> m ( SFTransformerSt m sc ) -- Realiza un 'tick' con la sección de chunk de entrada especificada, produciendo una sección de señal en la sección de chunk destino especificada, y devuelve el próximo estado
   , sftTickInplace :: (Monad m, SFSignalChunk m sc) => SignalChunkSection sc -> m ( SFTransformerSt m sc ) -- Realiza un 'tick', produce una sección de señal en la sección chunk especificada, tomándola como entrada y como salida, y devuelve el próximo estado
   , sftDelete :: (Monad m) => m () -- Destruye el transformador
   }
