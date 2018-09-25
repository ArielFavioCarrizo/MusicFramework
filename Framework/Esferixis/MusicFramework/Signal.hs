{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal
   (
     Sectionable(sLength)
   , Section(secSource, secOffset, secLength)
   , sWhole
   , sSubSection
   , SignalChunk(
       scLength
     , scSection
     , scAppend
     , scSplitRef
     , scZero
     )
   , scEmpty
   , scIsEmpty
   , scCheckSameLength
   , SignalProcessorState(
       spChunkLength
     , spReduceChunkLength
     )
   , makeSpPairConvert
   ) where

import Data.Word
import Data.Maybe

class Sectionable s where
   sLength :: s -> Word64

sWhole :: (Sectionable s) => s -> Section s
sWhole sectionable =
   Section {
        secSource = sectionable
      , secOffset = 0
      , secLength = sLength sectionable
      }

data Section s = Section {
     secSource :: (Sectionable s) => s
   , secOffset :: Word64
   , secLength :: Word64
   }

sSubSection :: (Sectionable s) => Section s -> Word64 -> Word64 -> Section s
sSubSection section offset length =
   let srcOffset = secOffset section
       srcLength = secLength section
   in
      if ( ( offset + length ) < srcLength )
         then
            Section {
                 secSource = secSource section
               , secOffset = srcOffset + offset
               , secLength = length
               }
         else error "Invalid interval"

{- 
   Representación abstracta de lo que es un chunk de señal
   Representa un chunk de señal que se obtendrá efectivamente
   en un tiempo indeterminado en el futuro

   En el caso de ser un chunk silencioso, la implementación podría
   optar por no crear un chunk. Y en su lugar podría ser una
   representación de una señal cero.

   Un chunk de señal de longitud cero se interpreta como el fin del stream
-}
class (Monad m) => SignalChunk m sc | sc -> m where
   scLength :: sc -> Word64 -- Longitud del chunk
   scSection :: sc -> Word64 -> Word64 -> m sc -- Devuelve la sección con el offset y la longitud especificados
   scAppend :: sc -> sc -> m sc -- Toma dos chunks y genera un chunk nuevo uniendo el primer chunk con el segundo
   scSplitRef :: sc -> m (sc, sc) -- Divide la referencia del chunk en dos referencias
   scZero :: Word64 -> m sc -- Genera un chunk silencioso con la longitud especificada

scEmpty :: (SignalChunk m sc) => m sc
scEmpty = scZero 0

scIsEmpty :: (SignalChunk m sc) => sc -> Bool
scIsEmpty signalChunk = (scLength signalChunk == 0 )

scCheckSameLength :: (SignalChunk m sc) => sc -> Word64 -> m ()
scCheckSameLength signalChunk expectedLength = do
   if ( scLength signalChunk == expectedLength )
      then return ()
      else fail "Unexpected signal chunk length"

-- Estado de unidad de procesamiento de señal
class SignalProcessorState a where
   spChunkLength :: a -> Word64 -- Longitud de datos a procesar. Cuando es cero significa que terminó el stream.
   spReduceChunkLength :: a -> Word64 -> a -- Reduce la longitud de datos a procesar

{-
   Genera una función de conversión de dos unidades de procesamiento de señal en una sola,
   cuya salida es una tupla con la salida de cada unidad de procesamiento
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
