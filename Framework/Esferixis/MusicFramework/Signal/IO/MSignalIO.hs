{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Esferixis.MusicFramework.Signal.IO.MSignalIO(
     MSignalIO(
          MSIOMaxChunkLength
        , MSIONewProducer
        , MSIOPopChunk
        , MSIONewConsumer
        , MSIOPushChunk
        , MSIONewTransformer
        , MSIOTransform
        , MSIODispose
        , MSIOBind
        , MSIOReturn
        , MSIOFail
        )
   , MSIODisposable
   , MSChunk(scLength)
   , MSChunkTuple
   , MSIOUnitGen
   , MSIOProducer
   , MSIOProducerTemplate
   , MSIOTransformer
   , MSIOTransformerTemplate
   , MSIOConsumer
   , MSIOConsumerTemplate
   ) where

import Data.Word
import Data.Maybe
import Control.Applicative
import Control.Monad

class MSIODisposable a

class (MSIODisposable fc) => MFramesChunk fc where
   fcLength :: fc -> Word64
   fcChannels :: fc -> Word32

class (MSIODisposable sc) => MSChunk sc where
   scLength :: sc -> Word64

class (MSChunk lsc, MSChunk rsc, MSChunk tsc) => MSChunkTuple tsc lsc rsc | lsc -> tsc, rsc -> tsc

class (MSIODisposable p) => MSIOUnitGen p

class (MSIOUnitGen p, MSChunk sc) => MSIOProducer p sc | p -> sc

class (MSIOProducer p sc) => MSIOProducerTemplate pt p sc

class (MSIOUnitGen t, MSChunk isc, MSChunk osc) => MSIOTransformer t isc osc | t -> isc, t -> osc

class (MSIOTransformer t isc osc) => MSIOTransformerTemplate tt t isc osc

class (MSChunk osc) => MSIOConsumer c osc | c -> osc

class (MSIOUnitGen c, MSIOConsumer c sc) => MSIOConsumerTemplate ct c sc

-- Acción con chunks de señales mutables
data MSignalIO r where
   -- Dado el UnitGen devuelve la longitud máxima garantizada que puede procesar de chunks de ahora para adelante. Si devuelve cero significa que no puede procesar más.
   MSIOMaxChunkLength :: (MSIOUnitGen ug) => ug -> MSignalIO Word64

   -- Crea un productor con el template especificado
   MSIONewProducer :: (MSIOProducerTemplate pt p sc) => pt -> MSignalIO p
   -- Produce en el chunk especificado
   MSIOPopChunk :: (MSIOProducer p sc) => p -> sc -> MSignalIO sc

   -- Crea un consumidor con el template especificado
   MSIONewConsumer :: (MSIOConsumerTemplate ct c sc) => ct -> MSignalIO c
   -- Empuja un chunk al consumidor especificado
   MSIOPushChunk :: (MSIOConsumer c sc) => c -> sc -> MSignalIO ()

   -- Crea un transformador con el template especificado
   MSIONewTransformer :: (MSIOTransformerTemplate tt t isc osc) => tt -> MSignalIO t
   -- Realiza una transformación con el transformador, el chunk de entrada y el chunk de salida especificados. Devuelve el chunk transformado.
   MSIOTransform :: (MSIOTransformer t isc osc) => t -> isc -> osc -> MSignalIO ()

   -- Termina el uso del objeto especificado
   MSIODispose :: (MSIODisposable a) => a -> MSignalIO ()

   -- Bind monádico
   MSIOBind :: MSignalIO s -> ( s -> MSignalIO r ) -> MSignalIO r

   -- Return monádico
   MSIOReturn :: r -> MSignalIO r

   -- Error monádico
   MSIOFail :: String -> MSignalIO r

instance Monad MSignalIO where
   preSignalIO >>= k = MSIOBind preSignalIO k

   return value = MSIOReturn value
   
   fail message = MSIOFail message

instance Applicative MSignalIO where
   pure = return
   (<*>) = ap

instance Functor MSignalIO where
   fmap = liftM
