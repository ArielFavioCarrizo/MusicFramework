{-# LANGUAGE Rank2Types #-}

module Esferixis.MusicFramework.Signal.Consumer
   ( ConsumerState(ConsumerState, csPushChunk) ) where

import Data.Word
import Data.Maybe
import Esferixis.MusicFramework.Signal

{- 
   Representación abstracta de lo que es un estado de consumidor.
   Representa un estado del consumidor que se obtendrá efectivamente
   en un tiempo indeterminado en el futuro.

   Tipos

   sc: Signal Chunk
-}
data ConsumerState sc = ConsumerState { csPushChunk :: (SignalChunk sc) => sc -> ConsumerState sc -- Empuja un 'chunk' de señal. Un chunk de longitud cero marca el fin del stream.
                                      }
