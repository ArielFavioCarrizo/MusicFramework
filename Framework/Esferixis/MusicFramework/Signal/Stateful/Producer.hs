{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Signal.Stateful.Producer
   (
     SFProducer(SFProducer, sfpNewInstance)
   , SFProducerSt(SFProducerSt, sfpMaxFrames, sfpTick, sfpDelete)
   ) where

import Data.Word
import Data.Maybe
import Esferixis.MusicFramework.Signal.Stateful.Signal
import Esferixis.Control.Concurrency.AsyncIO

{-
   Representación de productor stateful no manejado
-}
data SFProducer m sc = SFProducer {
     -- Crea una instancia del productor. Si el stream es vacío el productor no sea crea.
     sfpNewInstance :: SFProducer m sc -> m ( Maybe ( SFProducer m sc ) )
   }

{- 
   Representación abstracta de estado de productor stateful
   no manejado
-}
data SFProducerSt sc = SFProducerSt {
     sfpMaxFrames :: Word64 -- Máxima cantidad de frames que puede recibir
     {-
        Escribe en el chunk especificado y pasa al siguiente estado
        Si el stream de entrada se termina devuelve Nothing y se destruye
        el productor
     -}
   , sfpTick :: (SFSignalChunk sc) => sc -> AsyncIO ( Maybe ( SFProducerSt sc ) )
   , sfpDelete :: AsyncIO () -- Destruye el productor.
   }
