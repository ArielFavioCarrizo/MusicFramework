{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Esferixis.MusicFramework.Signal.IO.SignalIO(
     SignalIO(
          SIOMaxChunkLength
        , SIONewProducer
        , SIOPopChunk
        , SIONewConsumer
        , SIOPushChunk
        , SIONewTransformer
        , SIOTransform
        , SIOJoin
        , SIOSplit
        , SIOSection
        , SIOFuse
        , SIORemoveRef
        , SIOBind
        , SIOReturn
        , SIOFail
        )
   , SIODisposable
   , SChunk(scLength)
   , SChunkTuple
   , SIOUnitGen
   , SIOProducer
   , SIOProducerTemplate
   , SIOTransformer
   , SIOTransformerTemplate
   , SIOConsumer
   , SIOConsumerTemplate
   ) where

import Data.Word
import Data.Maybe
import Data.Proxy
import Control.Applicative
import Control.Monad

class SIODisposable a

class (SIODisposable sc) => SChunk sc where
   scLength :: sc -> Word64

class (SChunk lsc, SChunk rsc, SChunk tsc) => SChunkTuple tsc lsc rsc | lsc rsc -> tsc

class (SIODisposable p) => SIOUnitGen p

class (SIOUnitGen p, SChunk sc) => SIOProducer e p sc | e sc -> p

class (SIOProducer e p sc) => SIOProducerTemplate e pt p sc | e pt -> p sc

class (SIOUnitGen t, SChunk isc, SChunk osc) => SIOTransformer e t isc osc | e isc osc -> t

class (SIOTransformer e t isc osc) => SIOTransformerTemplate e tt t isc osc | e tt -> t isc osc

class (SChunk osc) => SIOConsumer e c osc | e osc -> c

class (SIOUnitGen c, SIOConsumer e c sc) => SIOConsumerTemplate e ct c sc | e ct -> c sc

-- Acción con señales, indicando el tipo del entorno, y el tipo de valor de retorno
data SignalIO e r where
   -- Dado el UnitGen devuelve la longitud máxima garantizada que puede procesar de chunks de ahora para adelante. Si devuelve cero significa que no puede procesar más.
   SIOMaxChunkLength :: (SIOUnitGen ug) => ug -> SignalIO e Word64

   -- Crea un productor con el template especificado
   SIONewProducer :: (SIOProducerTemplate e pt p sc) => pt -> SignalIO e p
   -- Devuelve un chunk con el productor y el tamaño de chunk deseado
   SIOPopChunk :: (SIOProducer e p sc) => p -> Word64 -> SignalIO e sc

   -- Crea un consumidor con el template especificado
   SIONewConsumer :: (SIOConsumerTemplate e ct c sc) => ct -> SignalIO e c
   -- Empuja un chunk al consumidor especificado
   SIOPushChunk :: (SIOConsumer e c sc) => c -> sc -> SignalIO e ()

   -- Crea un transformador con el template especificado
   SIONewTransformer :: (SIOTransformerTemplate e tt t isc osc) => tt -> SignalIO e t
   -- Realiza una transformación con el transformador y el chunk de entrada especificado. Devuelve el chunk transformado.
   SIOTransform :: (SIOTransformer e t isc osc) => t -> isc -> SignalIO e osc

   -- Une dos chunks en una tupla
   SIOJoin :: (SChunkTuple tsc lsc rsc) => lsc -> rsc -> SignalIO e tsc
   -- Divide la tupla de chunks, separándola en sus componentes
   SIOSplit :: (SChunkTuple tsc lsc rsc) => tsc -> SignalIO e (lsc, rsc)
   -- Devuelve una porción del chunk con el offset y el tamaño especificados
   SIOSection :: (SChunk sc) => sc -> Word64 -> Word64 -> SignalIO e sc
   -- Fusiona dos chunks. Del izquierdo al derecho.
   SIOFuse :: (SChunk sc) => sc -> sc -> SignalIO e sc

   -- Remueve la referencia al objeto especificado. La invalida. Después no puede volver a usarse.
   SIORemoveRef :: (SIODisposable a) => a -> SignalIO e ()

   -- Bind monádico
   SIOBind :: SignalIO e s -> ( s -> SignalIO e r ) -> SignalIO e r

   -- Return monádico
   SIOReturn :: r -> SignalIO e r

   -- Error monádico
   SIOFail :: String -> SignalIO e r

instance Monad (SignalIO e) where
   preSignalIO >>= k = SIOBind preSignalIO k

   return value = SIOReturn value
   
   fail message = SIOFail message

instance Applicative (SignalIO e) where
   pure = return
   (<*>) = ap

instance Functor (SignalIO e) where
   fmap = liftM
