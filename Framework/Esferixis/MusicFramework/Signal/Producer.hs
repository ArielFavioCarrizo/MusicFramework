module Esferixis.MusicFramework.Signal.Producer
   ( ProducerState(ProducerState, psMaxPopChunkLength, psPopChunk, psCloseSourceStream) ) where

import Data.Word
import Control.Concurrent.Async
import Data.Maybe
import Control.Concurrent.Async

data ProducerState dataChunk = ProducerState { psMaxPopChunkLength :: Word64 -- Máxima longitud de datos que se puede extraer
                                             , psPopChunk :: Word64 -> IO (Maybe (Async dataChunk), Maybe (ProducerState dataChunk)) -- Recibe la longitud deseada y devuelve potencialmente un 'chunk' de datos y la sección siguiente (Si no termina el stream)
                                             , psCloseSourceStream :: IO (Async ()) -- Cierra el stream de origen
                                             }

