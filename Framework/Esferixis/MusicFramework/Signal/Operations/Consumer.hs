{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Signal.Operations.Consumer(
     SFConsumer(sfcFirstState)
   , SFConsumerSt(
          SFReadyConsumerSt
        , sfcMaxFrames
        , sfcPushTickOp
        , sfcDoPendingOps
        , sfcTerminate
        , SFPendingOpsConsumerSt
        , SFTerminatedConsumerSt
        )
   ) where

import Data.Word
import Data.Maybe
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.MusicFramework.Signal.Operations.Signal

{-
   Representación de consumidor stateful
-}
data SFConsumer sc = SFConsumer { sfcFirstState :: SFConsumerSt sc }

{- 
   Descripción de estado de consumidor stateful
-}
data SFConsumerSt sc =
   -- Estado de consumidor listo para ordenarle operaciones
   SFReadyConsumerSt {
        sfcMaxFrames :: Word64 -- Máxima cantidad de frames con los que puede operar en el tick
        {-
           Agrega la operación de tick con el tamaño de chunk,
           la acción AsyncIO de recepción de chunk,
           y una función que recibe la acción que realiza
           la operación de tick.
           Ésta da el futuro de la compleción del tick, y la
           continuación del consumo.
           Dependiendo de la implementación, la continuación puede bloquear.
           Con éstos dos elementos construye una acción compuesta.

           Devuelve el próximo estado.
        -}
      , sfcPushTickOp :: (SFSignalChunk sc) => Word64 -> AsyncIO (Future sc) -> ( AsyncIO ( Future (), AsyncIO () ) -> AsyncIO () ) -> SFConsumerSt sc
        -- Realiza las acciones pendientes y devuelve el próximo estado
      , sfcDoPendingOps :: AsyncIO ( SFConsumerSt sc )
        {-
           Termina el uso del consumidor, devolviendo
           una acción que realiza todas las operaciones pendientes
        -}
      , sfcTerminate :: AsyncIO ()
      } |
   -- Estado de consumidor que necesita que se hagan las operaciones pendientes para avanzar
   SFPendingOpsConsumerSt ( AsyncIO ( SFConsumerSt sc ) ) |
   -- Estado de consumidor terminado. Contiene la acción asíncrona que realiza todas las acciones pendientes
   SFTerminatedConsumerSt ( AsyncIO () )
