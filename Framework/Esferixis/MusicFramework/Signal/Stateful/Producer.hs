{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal.Stateful.Producer
   ( SFProducerSt(sfpMaxChunkSecLength, sfpTickOp, sfpDelete) ) where

import Data.Word
import Data.Maybe

import Esferixis.MusicFramework.Signal.Stateful.SignalChunk

{- 
   Representación abstracta de un productor stateful
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
   , sfpTickOp :: (Monad m, SFSignalChunk m sc) => Word64 -> ( ( opIn -> SignalChunkSection sc -> m opIn ), SFProducerSt m sc opIn )
   , sfpDelete :: (Monad m) => m () -- Destruye el productor
   }
