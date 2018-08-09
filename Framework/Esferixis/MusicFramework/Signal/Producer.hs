module Esferixis.MusicFramework.Signal.Producer
   ( ProducerState(ProducerState)
   , StatefulProducer(StatefulProducer) ) where

import Data.Word
import Control.Concurrent.Async
import Data.Maybe

data ProducerState dataChunk = ProducerState { psMaxPopChunkLength :: Word32 -- Máxima longitud de datos que se puede extraer
                                             , psPopChunk :: Word32 -> IO (Maybe (Async dataChunk), Maybe (ProducerState dataChunk)) -- Recibe la longitud deseada y devuelve potencialmente un 'chunk' de datos y la sección siguiente (Si no termina el stream)
                                             , psCloseSourceStream :: IO () -- Cierra el stream de origen
                                             }

data StatefulProducer dataChunk = StatefulProducer { spActionsByDuration :: [(Word32, IO ())] -- Acciones por duración
                                                   , spTick :: Word32 -> IO dataChunk -- Produce un chunk de datos de la longitud especificada
                                                   , spClose :: IO () -- Cierra el productor
                                                   }
