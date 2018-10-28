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
        , MSIONewSymmetricTransformer
        , MSIOTransform
        , MSIONewBufferedTransformer
        , MSIORemoveRef
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
   , MSIOSymmetricTransformer
   , MSIOSymmetricTransformerTemplate
   , MSIOBufferedTransformer(msioInput, msioOutput)
   , MSIOBufferedTransformerTemplate
   , MSIOConsumer
   , MSIOConsumerTemplate
   ) where

import Data.Word
import Data.Maybe
import Data.Proxy
import Control.Applicative
import Control.Monad

class MSIODisposable a

class (MSIODisposable fc) => MFramesChunk e fc csc | e -> fc, fc -> csc where
   fcLength :: Proxy e -> fc -> Word64
   fcChannels :: Proxy e -> fc -> Word32

class (MSIODisposable sc) => MSChunk sc where
   scLength :: sc -> Word64

class (MSChunk lsc, MSChunk rsc, MSChunk tsc) => MSChunkTuple tsc lsc rsc | lsc rsc -> tsc

class (MSIODisposable p) => MSIOUnitGen p

class (MSIOUnitGen p, MSChunk sc) => MSIOProducer e p sc | e sc -> p

class (MSIOProducer e p sc) => MSIOProducerTemplate e pt p sc | e pt -> p sc

class (MSIOUnitGen t, MSChunk isc, MSChunk osc) => MSIOSymmetricTransformer e t isc osc | isc osc -> t

class (MSIOSymmetricTransformer e t isc osc) => MSIOSymmetricTransformerTemplate e tt t isc osc | e tt -> t isc osc

data MSIOBufferedTransformer e c p isc osc = MSIOBufferedTransformer {
     msioInput :: (MSIOConsumer e c isc) => c
   , msioOutput :: (MSIOProducer e p osc) => p
   }

class (MSIOConsumer e c isc, MSIOProducer e p osc, MSChunk isc, MSChunk osc) => MSIOBufferedTransformerTemplate e tt c p isc osc | e tt -> c p isc osc

class (MSChunk osc) => MSIOConsumer e c osc | e osc -> c

class (MSIOUnitGen c, MSIOConsumer e c sc) => MSIOConsumerTemplate e ct c sc | e ct -> c sc

-- Acción con chunks de señales mutables, indicando el tipo del entorno, y el tipo de valor de retorno
data MSignalIO e r where
   -- Dado el UnitGen devuelve la longitud máxima garantizada que puede procesar de chunks de ahora para adelante. Si devuelve cero significa que no puede procesar más.
   MSIOMaxChunkLength :: (MSIOUnitGen ug) => ug -> MSignalIO e Word64

   -- Crea un chunk de frames con la longitud y los canales especificados
   MSIONewFramesChunk :: (MFramesChunk e fc csc) => Word64 -> Word32 -> MSignalIO e fc
   -- Devuelve el chunk de señal del canal de chunk de frames especificado
   MSIOChannel :: (MFramesChunk e fc csc) => fc -> Word32 -> MSignalIO e csc

   -- Une dos chunks en una tupla
   MSIOJoin :: (MSChunkTuple tsc lsc rsc) => lsc -> rsc -> MSignalIO e tsc
   -- Devuelve una porción del chunk de señal con el offset y el tamaño especificados
   MSIOSection :: (MSChunk sc) => sc -> Word64 -> Word64 -> MSignalIO e sc
   -- Copia el contenido del primer chunk de señal al segundo. Ambos tienen que ser de la misma longitud.
   MSIOCopy :: (MSChunk sc) => sc -> sc -> MSignalIO e ()

   -- Crea un productor con el template especificado
   MSIONewProducer :: (MSIOProducerTemplate e pt p sc) => pt -> MSignalIO e p
   -- Produce en el chunk especificado
   MSIOPopChunk :: (MSIOProducer e p sc) => p -> sc -> MSignalIO e ()

   -- Crea un consumidor con el template especificado
   MSIONewConsumer :: (MSIOConsumerTemplate e ct c sc) => ct -> MSignalIO e c
   -- Empuja un chunk al consumidor especificado
   MSIOPushChunk :: (MSIOConsumer e c sc) => c -> sc -> MSignalIO e ()

   -- Crea un transformador simétrico con el template especificado
   MSIONewSymmetricTransformer :: (MSIOSymmetricTransformerTemplate e tt t isc osc) => tt -> MSignalIO e t
   -- Realiza una transformación con el transformador simétrico, el chunk de entrada y el chunk de salida especificados. Devuelve el chunk transformado.
   MSIOTransform :: (MSIOSymmetricTransformer e t isc osc) => t -> isc -> osc -> MSignalIO e ()

   -- Crea un transformador buffereado con el template especificado
   MSIONewBufferedTransformer :: (MSIOBufferedTransformerTemplate e tt c p isc osc) => tt -> MSignalIO e ( MSIOBufferedTransformer e c p isc osc )

   -- Remueve la referencia al objeto especificado. La invalida. Después no puede volver a usarse.
   MSIORemoveRef :: (MSIODisposable a) => a -> MSignalIO e ()

   -- Bind monádico
   MSIOBind :: MSignalIO e s -> ( s -> MSignalIO e r ) -> MSignalIO e r

   -- Return monádico
   MSIOReturn :: r -> MSignalIO e r

   -- Error monádico
   MSIOFail :: String -> MSignalIO e r

instance Monad (MSignalIO e) where
   preSignalIO >>= k = MSIOBind preSignalIO k

   return value = MSIOReturn value
   
   fail message = MSIOFail message

instance Applicative (MSignalIO e) where
   pure = return
   (<*>) = ap

instance Functor (MSignalIO e) where
   fmap = liftM
