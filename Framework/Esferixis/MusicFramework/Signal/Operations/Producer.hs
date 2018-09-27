{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Signal.Operations.Producer(
     SFProducer(SFProducer, sfpNewInstance)
   , SFProducerSt(SFProducerSt, sfpMaxFrames, sfpTick, sfpDelete)
   ) where

import Data.Word
import Data.Maybe
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.MusicFramework.Signal.Operations.Signal

{-
   Representación de productor stateful no manejado
-}
data SFProducer sc = SFProducer {
     -- Crea una instancia del productor. Si el stream es vacío el productor no sea crea.
     sfpNewInstance :: (SFSignalChunk sc) => SFProducer sc -> AsyncIO ( Maybe ( SFProducer sc ) )
   }

{- 
   Representación abstracta de estado de productor stateful
   no manejado
-}
data SFProducerSt sc = SFProducerSt {
     sfpMaxFrames :: Word64 -- Máxima cantidad de frames con los que puede operar en el tick
     {-
        Escribe en el chunk especificado y pasa al siguiente estado.
        Devuelve el futuro del resultado de la operación y el próximo estado.

        Si el stream de entrada se termina el próximo estado es Nothing y se destruye
        el productor.
     -}
   , sfpTick :: (SFSignalChunk sc) => sc -> AsyncIO ( Future (), Maybe (SFProducerSt sc) )
   , sfpDelete :: AsyncIO () -- Destruye el productor.
   }
