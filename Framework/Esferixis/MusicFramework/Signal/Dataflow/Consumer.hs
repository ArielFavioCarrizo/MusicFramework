{-# LANGUAGE Rank2Types #-}

module Esferixis.MusicFramework.Signal.Dataflow.Consumer
   ( ConsumerState(ConsumerState, csPushChunk) ) where

import Data.Word
import Data.Maybe
import Esferixis.MusicFramework.Signal.Dataflow.Misc

{- 
   Representación abstracta de lo que es un estado de consumidor.
   Representa un estado del consumidor que se obtendrá efectivamente
   en un tiempo indeterminado en el futuro.

   Tipos

   sc: Signal Chunk
-}
data ConsumerState m sc = ConsumerState { csPushChunk :: (Monad m, SignalChunk m sc) => sc -> m (ConsumerState m sc) -- Empuja un 'chunk' de señal. Un chunk de longitud cero marca el fin del stream.
                                      }
