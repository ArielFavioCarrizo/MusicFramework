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
        , SIODispose
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
import Control.Applicative
import Control.Monad

class SIODisposable a

class (SIODisposable sc) => SChunk sc where
   scLength :: sc -> Word64

class (SChunk lsc, SChunk rsc, SChunk tsc) => SChunkTuple tsc lsc rsc | lsc -> tsc, rsc -> tsc

class (SIODisposable p) => SIOUnitGen p

class (SIOUnitGen p, SChunk sc) => SIOProducer p sc | p -> sc

class (SIOProducer p sc) => SIOProducerTemplate pt p sc

class (SIOUnitGen t, SChunk isc, SChunk osc) => SIOTransformer t isc osc | t -> isc, t -> osc

class (SIOTransformer t isc osc) => SIOTransformerTemplate tt t isc osc

class (SChunk osc) => SIOConsumer c osc | c -> osc

class (SIOUnitGen c, SIOConsumer c sc) => SIOConsumerTemplate ct c sc

-- Acción con señales
data SignalIO r where
   -- Dado el UnitGen devuelve la longitud máxima garantizada que puede procesar de chunks de ahora para adelante. Si devuelve cero significa que no puede procesar más.
   SIOMaxChunkLength :: (SIOUnitGen ug) => ug -> SignalIO Word64

   -- Crea un productor con el template especificado
   SIONewProducer :: (SIOProducerTemplate pt p sc) => pt -> SignalIO p
   -- Devuelve un chunk con el productor y el tamaño de chunk deseado
   SIOPopChunk :: (SIOProducer p sc) => p -> Word64 -> SignalIO sc

   -- Crea un consumidor con el template especificado
   SIONewConsumer :: (SIOConsumerTemplate ct c sc) => ct -> SignalIO c
   -- Empuja un chunk al consumidor especificado
   SIOPushChunk :: (SIOConsumer c sc) => c -> sc -> SignalIO ()

   -- Crea un transformador con el template especificado
   SIONewTransformer :: (SIOTransformerTemplate tt t isc osc) => tt -> SignalIO t
   -- Realiza una transformación con el transformador y el chunk de entrada especificado. Devuelve el chunk transformado.
   SIOTransform :: (SIOTransformer t isc osc) => t -> isc -> SignalIO osc

   -- Une dos chunks en una tupla
   SIOJoin :: (SChunkTuple tsc lsc rsc) => lsc -> rsc -> SignalIO tsc
   -- Divide la tupla de chunks, separándola en sus componentes
   SIOSplit :: (SChunkTuple tsc lsc rsc) => tsc -> SignalIO (lsc, rsc)
   -- Devuelve una porción del chunk con el offset y el tamaño especificados
   SIOSection :: (SChunk sc) => sc -> Word64 -> Word64 -> SignalIO sc
   -- Fusiona dos chunks. Del izquierdo al derecho.
   SIOFuse :: (SChunk sc) => sc -> sc -> SignalIO sc

   -- Termina el uso del objeto especificado
   SIODispose :: (SIODisposable a) => a -> SignalIO ()

   -- Bind monádico
   SIOBind :: SignalIO s -> ( s -> SignalIO r ) -> SignalIO r

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
