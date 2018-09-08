{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal.Stateful.SignalChunk
   ( SFSignalChunk(sfscLength, sfscCopy, sfscNewZero), SignalChunkSection(scsSrc, scsOffset, scsLength) ) where

import Data.Word

{- 
   Representación abstracta de un chunk de señal
   stateful
-}
class (Monad m) => SFSignalChunk m sc | sc -> m where
   sfscLength :: sc -> Word64 -- Longitud del chunk
   sfscCopy :: sc -> Word64 -> Word64 -> Word64 -> m () -- Dado un chunk destino, el offset de origen, el offset de destino y la longitud a copiar, copia los datos del primer chunk al segundo
   sfscNewZero :: Word64 -> m sc -- Genera un chunk silencioso con la longitud especificada
   sfscDelete :: sc -> m () -- Destruye el chunk

{-
   Representación abstracta de una sección de chunk
-}
data SignalChunkSection sc = SignalChunkSection {
     scsSrc :: sc
   , scsOffset :: Word64
   , scsLength :: Word64
   }
