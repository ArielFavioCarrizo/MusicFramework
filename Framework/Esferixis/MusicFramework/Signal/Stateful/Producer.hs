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
     -- Crea una instancia del productor. Si el stream es vacío el productor no sea crea.
     sfpNewInstance :: SFProducer m sc -> m ( Maybe ( SFProducer m sc ) )
   }

{- 
   Representación abstracta de estado de productor stateful
   no manejado
-}
data SFProducerSt m sc = SFProducerSt {
     sfpMaxChunkSecLength :: Word64 -- Máximo tamaño de chunk que puede recibir
     {-
        Escribe en el chunk especificado y pasa al siguiente estado
        Si el stream de entrada se termina devuelve Nothing y se destruye
        el productor
     -}
   , sfpTickOp :: (Monad m) => m ( sc, Maybe ( SFProducerSt m sc ) )
   , sfpDelete :: (Monad m) => m () -- Destruye el productor.
   }
