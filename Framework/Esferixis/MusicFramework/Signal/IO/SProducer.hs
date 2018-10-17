{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFramework.Signal.IO.SProducer(
     SProducerSt(SProducerSt)
   ) where

import Data.Word
import Data.Maybe
import Data.Functor
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.Control.Concurrency.SharedRef
import Esferixis.MusicFramework.Signal.Misc
import Esferixis.MusicFramework.Signal.Operations.Signal

{-
   Representación de generador de señal
-}
data SProducerSt ch = SProducerSt {
     -- Máxima cantidad de frames que se pueden procesar como mínimo
     sMaxFrames :: Word64
     -- Realiza un tick de la longitud especificada, devolviendo el futuro del chunk convertido y el próximo estado.
   , sTick :: Word64 -> AsyncIO ( Future ( SharedRef ch ), Maybe ( SProducerSt ch ) )
     -- Termina el uso del productor
   , sTerminate :: AsyncIO ()
   }
