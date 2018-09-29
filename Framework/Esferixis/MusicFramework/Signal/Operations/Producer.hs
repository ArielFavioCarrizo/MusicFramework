{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Signal.Operations.Producer(
     SFProducer(sfpFirstState)
   , SFProducerSt(
          SFReadyProducerSt
        , sfpMaxFrames
        , sfpPushTickOp
        , sftDoPendingOps
        , sfpTerminate
        , SFPendingOpsProducerSt
        , SFTerminatedProducerSt
        )
   ) where

import Data.Word
import Data.Maybe
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.MusicFramework.Signal.Operations.Signal

{-
   Representación de productor stateful
-}
data SFProducer sc = SFProducer { sfpFirstState :: SFProducerSt sc }

{- 
   Representación abstracta de estado de productor stateful
   no manejado
-}
data SFProducerSt sc =
   -- Estado de productor listo para ordenarle operaciones
   SFReadyProducerSt {
        sfpMaxFrames :: Word64 -- Máxima cantidad de frames con los que puede operar en el tick
        {-
           Agrega la operación de tick con el tamaño de chunk,
           la acción AsyncIO de recepción de operación de tick,
           y una función que recibe una acción que se realiza
           cuando termina de realizarse el tick y da
           como resultado otra acción AsyncIO.

           Devuelve el próximo estado.
        -}
      , sfpPushTickOp :: (SFSignalChunk sc) => Word64 -> AsyncIO ( sc ) -> ( AsyncIO ( Future () ) -> AsyncIO () ) -> SFProducerSt sc
        -- Realiza las acciones pendientes y devuelve el próximo estado
      , sftDoPendingOps :: AsyncIO ( SFProducerSt sc )
        {-
           Termina el uso del transformador, devolviendo
           una acción que realiza todas las operaciones pendientes
        -}
      , sfpTerminate :: AsyncIO ()
      } |
   -- Estado de productor que necesita que se hagan las operaciones pendientes para avanzar
   SFPendingOpsProducerSt ( AsyncIO ( SFProducerSt sc ) ) |
   -- Estado de productor terminado. Contiene la acción asíncrona que realiza todas las acciones pendientes
   SFTerminatedProducerSt ( AsyncIO () )
