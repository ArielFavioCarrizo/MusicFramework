{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFramework.Signal.Operations.Producer(
     SFProducer(sfpNewInstance)
   , SFProducerSt(
          SFProducerSt
        , sfpMaxFrames
        , sfpPushTickOp
        , sfpTerminate
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
data SFProducer sc = SFProducer { sfpNewInstance :: AsyncIO ( Maybe ( SFProducerSt sc ) ) }

{- 
   Representación abstracta de estado de productor stateful
   no manejado
-}
data SFProducerSt sc =
   SFProducerSt {
        -- Máxima cantidad de frames garantizada con los que puede operar en el tick y subsiguientes.
        sfpMaxFrames :: AsyncIO Word64
        -- Realiza una operación con el chunk especificado.
      , sfpPushTickOp :: sc -> AsyncIO ()
        -- Termina el uso del productor
      , sfpTerminate :: AsyncIO ()
      }

instance SFSignalUnitSt (SFProducerSt sc) sc where
   sfsuMaxFrames = sfpMaxFrames
   sfsuPushTickOp = sfpPushTickOp
   sfsuTerminate = sfpTerminate
