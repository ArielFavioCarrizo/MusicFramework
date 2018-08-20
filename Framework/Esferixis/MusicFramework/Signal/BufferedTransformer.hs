{-# LANGUAGE Rank2Types #-}

module Esferixis.MusicFramework.Signal.BufferedTransformer
   ( BufferedTransformerOutputState(btsPopChunkLength, btsReducePopChunkLength, btsPopChunk), BufferedTransformerState(btsOutput, btsPushChunk) ) where

import Data.Word
import Data.Maybe
import Esferixis.MusicFramework.Signal
import Esferixis.MusicFramework.Signal.Producer
import Esferixis.MusicFramework.Signal.Transformer

{- 
   Representación abstracta de lo que es un transformador con buffering.
   Representa un estado del transformador que se obtendrá efectivamente
   en un tiempo indeterminado en el futuro.

   Tipos

   isc: Input Signal Chunk
   osc: Output Signal Chunk
-}
data BufferedTransformerOutputState isc osc = BufferedTransformerOutputState {
     btsPopChunkLength :: Word64 -- Longitud de datos a extraer
   , btsReducePopChunkLength :: Word64 -> BufferedTransformerState isc osc
   , btsPopChunk :: (SignalChunk osc) => (osc, BufferedTransformerState isc osc)
   }

data BufferedTransformerState isc osc = TransformerState {
     btsOutput :: Maybe (BufferedTransformerOutputState isc osc)
   , btsPushChunk :: (SignalChunk isc) => isc -> BufferedTransformerState isc osc -- Recibe el chunk de señal de entrada. Si recibe un chunk de señal de longitud cero, significa que se terminó el stream de entrada.
   }
