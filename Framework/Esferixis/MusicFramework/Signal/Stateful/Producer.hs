{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Signal.Stateful.Producer
   (
     SFProducer(SFProducer, sfpNewInstance)
   , SFProducerSt(SFProducerSt, sfpMaxChunkSecLength, sfpTickOp, sfpDelete)
   ) where

import Data.Word
import Data.Maybe

{-
   Representación de productor stateful no manejado
-}
data SFProducer m sc = SFProducer {
     sfpNewInstance :: SFProducer m sc -> m ( SFProducer m sc )
   }

{- 
   Representación abstracta de estado de productor stateful
   no manejado
-}
data SFProducerSt m sc = SFProducerSt {
     sfpMaxChunkSecLength :: Word64 -- Máximo tamaño de chunk que puede recibir
     -- Escribe en el chunk especificado y pasa al siguiente estado
   , sfpTickOp :: (Monad m) => m ( sc, SFProducerSt m sc )
   , sfpDelete :: (Monad m) => m () -- Destruye el productor
   }
