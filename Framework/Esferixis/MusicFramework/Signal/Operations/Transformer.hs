{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Signal.Operations.Transformer(
     SFTransformer(SFTransformer, sftFirstState)
   , SFTransformerTickOp(
          SFTransformerPureTickOp
        , SFTransformerInplaceTickOp
        )
   , SFTransformerSt(
          sftMaxFrames
        , sftTickOp
        , sftDeleteOp
        )
   ) where

import Data.Word
import Data.Maybe
import Control.Exception
import Control.Monad.IO.Class
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.MusicFramework.Signal.Misc
import Esferixis.MusicFramework.Signal.Operations.Signal

{-
   Representación de transformador stateful no manejado
-}
data SFTransformer sc = SFTransformer { sftFirstState :: Maybe ( SFTransformerSt sc ) }

{-
   Operación de transformación de sección de chunk
-}
data SFTransformerTickOp sc = SFTransformerPureTickOp ( SFSignalChunkIO sc ) | SFTransformerInplaceTickOp sc

data SFTransformerSt sc = SFTransformerSt {
     -- Máxima cantidad de frames con los que puede operar en el tick
     sftMaxFrames :: Word64
     -- Agrega una operación de tick con el tamaño de chunk especificado
   , sftTickOp :: (SFSignalChunk sc) => Word64 -> SFTransformerOp sc
     -- Agrega una operación de eliminación del transformador
   , sftDeleteOp :: SFTransformerOp sc
   }

data SFTransformerOp sc =
   {-
      Operación de transformado de frames con la operación de tick, y el futuro de operación
      anterior sobre el chunk especificado.
      Devuelve el futuro del resultado de la operación y el próximo estado.

      Si la transformación de señal termina devuelve Nothing
      y se destruye el transformador.
   -}
   SFTransformerTickOp ( (SFSignalChunk sc) => ( SFTransformerTickOp sc, Future () ) -> AsyncIO ( Future (), Maybe (SFTransformerOp sc) ) ) |
   -- Elimina el transformador
   SFTransformerDeleteOp ( AsyncIO () )
