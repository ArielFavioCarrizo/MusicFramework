{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFramework.Signal.IO.FramesChunk(
     SFFramesChunk(
          sffcMaxLength
        , sffcNew
        , sffcLength
        , sffcChannels
        , sffcCopyMono
        , sffcDelete
        )
   , SFFramesChunkChannel(SFFramesChunkChannel)
   , sffcChannel
   ) where

import Data.Word
import Data.Maybe
import Data.Functor
import Type.Reflection
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.MusicFramework.Signal.Misc
import Esferixis.MusicFramework.Signal.Operations.Signal

{-
   Representación de chunk de frames stateful
-}
class SFFramesChunk fc where
   sffcMaxLength :: TypeRep fc -> Word64 -- Tamaño máximo de chunk
   sffcNew :: Word64 -> Word32 -> IO fc -- Crea un chunk con la longitud y los canales especificados
   sffcLength :: fc -> Word64 -- Devuelve la longitud
   sffcChannels :: fc -> Word32 -- Devuelve la cantidad de canales
   sffcCopyMono :: SFFramesChunkChannel fc -> SFFramesChunkChannel fc -> Word64 -> Word64 -> Word64 -> IO () -- Copia el contenido del canal del primer chunk al canal del segundo con el offset origen, el offset destino y la longitud especificados
   sffcDelete :: fc -> IO () -- Destruye el chunk de frames

-- Canal de chunk de frames
data SFFramesChunkChannel fc = SFFramesChunkChannel {
     sffcSrcChunk :: (SFFramesChunk fc) => fc
   , sffcChannelNumber :: Word32
   }

instance (SFFramesChunk fc) => Sectionable (SFFramesChunkChannel fc) where
   sLength chunk = sffcLength $ sffcSrcChunk chunk

-- Obtención del canal
sffcChannel :: (SFFramesChunk fc) => fc -> Word32 -> SFFramesChunkChannel fc
sffcChannel chunk nChannel =
   if ( nChannel < (sffcChannels chunk) )
      then SFFramesChunkChannel {
               sffcSrcChunk = chunk
             , sffcChannelNumber = nChannel
             }
      else error "Invalid channel number"

instance SFSignalChunk (Section (SFFramesChunkChannel fc)) where
   scLength = secLength

