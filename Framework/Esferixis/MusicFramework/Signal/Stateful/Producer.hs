{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Signal.Stateful.Producer
   (
     SFProducer(SFProducer, sfpFirstState, sfpNewInstance)
   , SFProducerSt(SFProducerSt, sfpMaxChunkSecLength, sfpTickOp, sfpDelete)
   ) where

import Data.Word
import Data.Maybe

{-
   Representación de productor stateful
-}
data SFProducer m sc opIn = SFProducer {
     -- Primer estado del productor
     sfpFirstState :: SFProducer m sc opIn
     -- Crea una instancia del productor, devolviendo la entrada para la primera operación
   , sfpNewInstance :: m opIn
   }

{- 
   Representación abstracta de estado de productor stateful
   no manejado
   
   ATENCIÓN: Toda operación debe ser realizada en el orden
             especificado.

             Toda operación de estado posterior, tiene que ser posterior
             al estado anterior.

             Caso contrario se producirá comportamiento indefinido.
-}
data SFProducerSt m sc opIn = SFProducerSt {
     sfpMaxChunkSecLength :: Word64 -- Máximo tamaño de chunk que puede recibir
     -- Crea una acción de procesado de chunk con el tamaño especificado y pasa al siguiente estado
   , sfpTickOp :: (Monad m) => Word64 -> ( ( opIn -> sc -> m opIn ), SFProducerSt m sc opIn )
   , sfpDelete :: (Monad m) => m () -- Destruye el productor
   }
