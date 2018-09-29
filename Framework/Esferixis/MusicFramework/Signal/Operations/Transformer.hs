{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Signal.Operations.Transformer(
     SFTransformer(SFTransformer, sftFirstState)
   , SFTransformerTickOp(
          SFTransformerPureTickOp
        , SFTransformerInplaceTickOp
        )
   , SFTransformerSt(
          SFReadyTransformerSt
        , SFTerminatedTransformerSt
        , sftMaxFrames
        , sftPushTickOp
        , sftDoPendingOps
        , sftTerminate
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
   Representación de transformador puro con operaciones stateful
-}
data SFTransformer sc = SFTransformer { sftFirstState :: SFTransformerSt sc }

{-
   Operación de transformación de chunk
-}
data SFTransformerTickOp sc = SFTransformerPureTickOp ( SFSignalChunkIO sc ) | SFTransformerInplaceTickOp sc

-- Descripción de estado del transformador puro, con operaciones stateful
data SFTransformerSt sc = 
   -- Estado de transformador sin terminar
   SFReadyTransformerSt {
        -- Máxima cantidad de frames con los que puede operar en el tick
        sftMaxFrames :: Word64
        {-
           Agrega la operación de tick con el tamaño de chunk,
           la acción AsyncIO de recepción de operación de tick,
           y una función que dado el resultado del procesamiento
           devuelve otra acción AsyncIO a realizar.

           Devuelve el próximo estado.
         -}
      , sftPushTickOp :: (SFSignalChunk sc) => Word64 -> AsyncIO ( SFTransformerTickOp sc ) -> ( FutureValue () -> AsyncIO () ) -> SFTransformerSt sc
        -- Realiza las acciones pendientes y devuelve el próximo estado
      , sftDoPendingOps :: AsyncIO ( SFTransformerSt sc )
        {-
           Termina el uso del transformador, devolviendo
           una acción que realiza todas las operaciones pendientes
         -}
      , sftTerminate :: AsyncIO ()
      } |
   -- Estado de transformador terminado. Contiene la acción asíncrona que realiza todas las acciones pendientes.
   SFTerminatedTransformerSt ( AsyncIO () )
