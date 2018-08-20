{-# LANGUAGE Rank2Types #-}

module Esferixis.MusicFramework.Signal
   ( SignalChunk(scLength), SignalProcessorState(spChunkLength, spReduceChunkLength), makeSpPairConvert) where

import Data.Word
import Data.Maybe

{- 
   Representación abstracta de lo que es un chunk de señal
   Representa un chunk de señal que se obtendrá efectivamente
   en un tiempo indeterminado en el futuro

   En el caso de ser un chunk silencioso, la implementación podría
   optar por no crear un chunk. Y en su lugar podría ser una
   representación de una señal cero.

   Un chunk de señal de longitud cero se interpreta como el fin del stream
-}
class SignalChunk sc where
   scLength :: sc -> Word64 -- Longitud del chunk
   scSection :: sc -> Word64 -> Word64 -> sc -- Devuelve la sección con el offset y la longitud especificados
   scAppend :: sc -> sc -> sc -- Toma dos chunks y genera un chunk nuevo uniendo el primer chunk con el segundo
   scZero :: Word64 -> sc -- Genera un chunk silencioso con la longitud especificada

-- Estado de unidad de procesamiento de señal
class SignalProcessorState a where
   spChunkLength :: a -> Word64 -- Longitud de datos a procesar. Cuando es cero significa que terminó el stream.
   spReduceChunkLength :: a -> Word64 -> a -- Reduce la longitud de datos a procesar

{-
   Genera una función de conversión de dos transformadores de señal en un sólo,
   cuya salida es una tupla con la salida de cada transformador, y la entrada
   se dirige a los dos transformadores de entrada
-}
makeSpPairConvert :: (SignalProcessorState spa, SignalProcessorState spb, SignalProcessorState spc) => (SignalProcessorState spa, SignalProcessorState spb, SignalProcessorState spc) => ( Word64 -> (Word64 -> spc) -> spa -> spb -> spc ) -> spa -> spb -> spc
makeSpPairConvert generateBalancedUnit spStateA spStateB =
   let spAChunkLength = spChunkLength spStateA
       spBChunkLength = spChunkLength spStateB
       spAReduceChunkLength = spReduceChunkLength spStateA
       spBReduceChunkLength = spReduceChunkLength spStateB
       spPairConvert = makeSpPairConvert generateBalancedUnit
   in case ( compare spAChunkLength spBChunkLength ) of
      GT -> spPairConvert ( spAReduceChunkLength spBChunkLength ) spStateB
      LT -> spPairConvert spStateA ( spBReduceChunkLength spAChunkLength )
      EQ -> let chunkLength = spAChunkLength
                reduceChunkLength = \requestedChunkLength -> spPairConvert ( spAReduceChunkLength requestedChunkLength ) ( spBReduceChunkLength requestedChunkLength )
            in generateBalancedUnit chunkLength reduceChunkLength spStateA spStateB
