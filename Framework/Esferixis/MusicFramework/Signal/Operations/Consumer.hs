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
        -- Máxima cantidad de frames con los que puede operar en el tick. Los restantes son la cota superior mínima garantizada.
        sfcMaxFrames :: Word64
        -- Devuelve una acción que realiza la operación de tick con el chunk especificado. Devuelve el próximo estado.
      , sfcPushTickOp :: sc -> AsyncIO ( Maybe ( SFConsumerSt sc ) )
        -- Termina el uso del consumidor
      , sfcTerminate :: AsyncIO ()
      }

instance SFSignalUnitSt (SFConsumerSt sc) sc where
   sfsuMaxFrames = sfcMaxFrames
   sfsuPushTickOp = sfcPushTickOp
   sfsuTerminate = sfcTerminate
