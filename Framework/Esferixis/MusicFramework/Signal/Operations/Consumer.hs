{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFramework.Signal.Operations.Consumer(
     SFConsumer(sfcNewInstance)
   , SFConsumerSt(
          SFConsumerSt
        , sfcMaxFrames
        , sfcPushTickOp
        , sfcTerminate
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
data SFConsumer sc = SFConsumer { sfcNewInstance :: AsyncIO ( Maybe ( SFConsumerSt sc ) ) }

{- 
   Descripción de estado de consumidor stateful
-}
data SFConsumerSt sc =
   SFConsumerSt {
        -- Máxima cantidad de frames garantizada con los que puede operar en el tick y subsiguientes.
        sfcMaxFrames :: AsyncIO Word64
        -- Devuelve una acción que realiza la operación de tick con el chunk especificado.
      , sfcPushTickOp :: Future sc -> AsyncIO ()
        -- Termina el uso del consumidor
      , sfcTerminate :: AsyncIO ()
      }

instance SFSignalUnitSt (SFConsumerSt sc) (Future sc) where
   sfsuMaxFrames = sfcMaxFrames
   sfsuPushTickOp = sfcPushTickOp
   sfsuTerminate = sfcTerminate
