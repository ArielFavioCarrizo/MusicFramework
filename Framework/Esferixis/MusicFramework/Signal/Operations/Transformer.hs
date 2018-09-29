{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Signal.Operations.Transformer(
     SFTransformer(SFTransformer, sftNewInstance)
   , SFTransformerTickOp(
          SFTransformerPureTickOp
        , SFTransformerInplaceTickOp
        )
   , SFTransformerSt(
          SFTransformerSt
        , sftMaxFrames
        , sftTick
        , sftDelete
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
data SFTransformer sc = SFTransformer { sftNewInstance :: AsyncIO ( Maybe ( SFTransformerSt sc ) ) }

{-
   Operación de transformación de sección de chunk
-}
data SFTransformerTickOp sc = SFTransformerPureTickOp ( SFSignalChunkIO sc ) | SFTransformerInplaceTickOp sc

data SFTransformerSt sc = SFTransformerSt {
     -- Máxima cantidad de frames con los que puede operar en el tick
     sftMaxFrames :: Word64
     {-
        Operación de transformado de frames con la operación de tick, y el futuro de operación
	anterior sobre el chunk especificado.
        Devuelve el futuro del resultado de la operación y el próximo estado.

        Si la transformación de señal termina devuelve Nothing
        y se destruye el transformador.
     -}
   , sftTick :: (SFSignalChunk sc) => ( SFTransformerTickOp sc, Future () ) -> AsyncIO ( Future (), Maybe (SFTransformerSt sc) )
     -- Destruye el transformador
   , sftDelete :: AsyncIO ()
   }
