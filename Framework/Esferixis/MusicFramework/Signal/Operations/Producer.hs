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
        -- Máxima cantidad de frames con los que puede operar en el tick. Los restantes son la cota superior mínima garantizada.
        sfpMaxFrames :: Word64
        -- Realiza una operación con el chunk especificado. Devuelve el próximo estado.
      , sfpPushTickOp :: sc -> AsyncIO ( Maybe ( SFProducerSt sc ) )
        -- Termina el uso del productor
      , sfpTerminate :: AsyncIO ()
      }

instance SFSignalUnitSt (SFProducerSt sc) sc where
   sfsuMaxFrames = sfpMaxFrames
   sfsuPushTickOp = sfpPushTickOp
   sfsuTerminate = sfpTerminate
