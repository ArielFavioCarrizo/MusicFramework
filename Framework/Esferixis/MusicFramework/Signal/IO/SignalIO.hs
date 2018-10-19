{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Esferixis.MusicFramework.Signal.IO.SignalIO(
     SignalIO(
          SIONewProducer
        , SIOPopChunk
        , SIODisposeProducer
        , SIONewConsumer
        , SIOPushChunk
        , SIODisposeConsumer
        , SIONewTransformer
        , SIOTransform
        , SIODisposeTransformer
        , SIOJoin
        , SIOSplit
        , SIOSection
        , SIODisposeChunk
        , SIOBind
        , SIOReturn
        , SIOFail
        )
   , SignalChunk(scLength)
   , SignalUnitGen
   , SignalProducer
   , SignalProducerTemplate
   , SignalTransformer
   , SignalTransformerTemplate
   , SignalConsumer
   , SignalConsumerTemplate
   , SignalChunkTuple
   ) where

import Data.Word
import Data.Maybe
import Control.Applicative
import Control.Monad

class SignalChunk sc where
   scLength :: sc -> Word64

class SignalUnitGen p

class (SignalUnitGen p, SignalChunk sc) => SignalProducer p sc | p -> sc

class (SignalProducer p sc) => SignalProducerTemplate pt p sc

class (SignalUnitGen t, SignalChunk isc, SignalChunk osc) => SignalTransformer t isc osc | t -> isc, t -> osc

class (SignalTransformer t isc osc) => SignalTransformerTemplate tt t isc osc

class (SignalChunk osc) => SignalConsumer c osc | c -> osc

class (SignalUnitGen c, SignalConsumer c sc) => SignalConsumerTemplate ct c sc

class (SignalChunk lsc, SignalChunk rsc, SignalChunk tsc) => SignalChunkTuple tsc lsc rsc | lsc -> tsc, rsc -> tsc

-- Acción con señales
data SignalIO r where 
   -- Dado el UnitGen devuelve la longitud máxima garantizada que puede procesar de chunks de ahora para adelante. Si devuelve cero significa que no puede procesar más.
   SIOMaxChunkLength :: (SignalUnitGen ug) => ug -> SignalIO Word64

   -- Crea un productor con el template especificado
   SIONewProducer :: (SignalProducerTemplate pt p sc) => pt -> SignalIO p
   -- Devuelve un chunk con el productor y el tamaño de chunk deseado
   SIOPopChunk :: (SignalProducer p sc) => p -> Word64 -> SignalIO sc
   -- Termina el uso del productor especificado
   SIODisposeProducer :: (SignalProducer p sc) => p -> SignalIO ()

   -- Crea un consumidor con el template especificado
   SIONewConsumer :: (SignalConsumerTemplate ct c sc) => ct -> SignalIO c
   -- Empuja un chunk al consumidor especificado
   SIOPushChunk :: (SignalConsumer c sc) => c -> sc -> SignalIO ()
   -- Termina el uso del consumidor
   SIODisposeConsumer :: (SignalConsumer c sc) => c -> SignalIO ()

   -- Crea un transformador con el template especificado
   SIONewTransformer :: (SignalTransformerTemplate tt t isc osc) => tt -> SignalIO t
   -- Realiza una transformación con el transformador y el chunk de entrada especificado. Devuelve el chunk transformado.
   SIOTransform :: (SignalTransformer t isc osc) => t -> isc -> SignalIO osc
   -- Termina el uso del transformador especificado
   SIODisposeTransformer :: (SignalTransformer t isc osc) => t -> SignalIO ()

   -- Une dos chunks en una tupla
   SIOJoin :: (SignalChunkTuple tsc lsc rsc) => lsc -> rsc -> SignalIO tsc
   -- Divide la tupla de chunks, separándola en sus componentes
   SIOSplit :: (SignalChunkTuple tsc lsc rsc) => tsc -> SignalIO (lsc, rsc)
   -- Devuelve una porción del chunk con el offset y el tamaño especificados
   SIOSection :: (SignalChunk sc) => sc -> Word64 -> Word64 -> SignalIO sc
   -- Termina el uso del chunk
   SIODisposeChunk :: (SignalChunk sc) => sc -> SignalIO ()

   -- Bind monádico
   SIOBind :: SignalIO a -> ( a -> SignalIO r ) -> SignalIO r

   -- Return monádico
   SIOReturn :: r -> SignalIO r

   -- Error monádico
   SIOFail :: String -> SignalIO r

instance Monad SignalIO where
   preSignalIO >>= k = SIOBind preSignalIO k

   return value = SIOReturn value
   
   fail message = SIOFail message

instance Applicative SignalIO where
   pure = return
   (<*>) = ap

instance Functor SignalIO where
   fmap = liftM
