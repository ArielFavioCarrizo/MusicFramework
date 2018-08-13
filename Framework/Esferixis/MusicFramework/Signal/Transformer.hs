module Esferixis.MusicFramework.Signal.Transformer
   ( TransformerState(TransformerState, tsMaxChunkLength, tsTransform) ) where

import Data.Word
import Data.Maybe
import Esferixis.MusicFramework.Signal

{- 
   Representación abstracta de lo que es un transformador
   Representa un estado del transformador que se obtendrá efectivamente
   en un tiempo indeterminado en el futuro
-}
data TransformerState sd = TransformerState { tsMaxChunkLength :: Word64 -- Máxima longitud de datos que se puede transformar
                                            , tsTransform :: SignalChunk sd -> (SignalChunk sd, Maybe (TransformerState sd)) -- Recibe la longitud deseada, devuelve un 'chunk' de señal y la sección siguiente (Si no termina el stream)
                                            }
