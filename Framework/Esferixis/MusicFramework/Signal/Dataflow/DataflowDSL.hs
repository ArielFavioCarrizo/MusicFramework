{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Esferixis.MusicFramework.Signal.Dataflow.DataflowDSL(
     DataflowInstr(
          DIPopChunk
        , DIDisposeProducer
        , DIPushChunk
        , DIDisposeConsumer
        , DITransform
        , DIDisposeTransformer
        , DIJoin
        , DISplit
        , DISection
        , DIDisposeChunk
        , DIEnd
        )
   , DataflowChunk(dfcLength)
   , DataflowUnitGen
   , DataflowProducer
   , DataflowProducerTemplate
   , DataflowTransformer
   , DataflowTransformerTemplate
   , DataflowConsumer
   , DataflowConsumerTemplate
   , DataflowChunkTuple
   ) where

import Data.Word
import Data.Maybe
import Control.Monad

class DataflowChunk sc where
   dfcLength :: sc -> Word64

class DataflowUnitGen p

class (DataflowUnitGen p, DataflowChunk sc) => DataflowProducer p sc | p -> sc

class (DataflowProducer p sc) => DataflowProducerTemplate pt p sc | pt -> p

class (DataflowUnitGen t, DataflowChunk isc, DataflowChunk osc) => DataflowTransformer t isc osc | t -> isc, t -> osc

class (DataflowTransformer t isc osc) => DataflowTransformerTemplate tt t isc osc | tt -> t

class (DataflowChunk osc) => DataflowConsumer c osc | c -> osc

class (DataflowUnitGen c, DataflowConsumer c sc) => DataflowConsumerTemplate ct c sc | ct -> c

class (DataflowChunk lsc, DataflowChunk rsc, DataflowChunk tsc) => DataflowChunkTuple tsc lsc rsc | lsc -> tsc, rsc -> tsc

data DataflowInstr where 
   -- Dado el UnitGen devuelve la longitud máxima garantizada que puede procesar de chunks de ahora para adelante. Si devuelve cero significa que no puede procesar más.
   DIMaxChunkLength :: (DataflowUnitGen ug) => ug -> ( Word64 -> DataflowInstr ) -> DataflowInstr

   -- Crea un productor con el template especificado. Devuelve el productor.
   DINewProducer :: (DataflowProducerTemplate pt p sc) => pt -> ( p -> DataflowInstr ) -> DataflowInstr
   -- Devuelve un chunk con el productor y el tamaño de chunk deseado
   DIPopChunk :: (DataflowProducer p sc) => p -> Word64 -> ( sc -> DataflowInstr ) -> DataflowInstr
   -- Termina el uso del productor especificado
   DIDisposeProducer :: (DataflowProducer p sc) => p -> DataflowInstr -> DataflowInstr

   -- Crea un consumidor con el template especificado. Devuelve el consumidor.
   DINewConsumer :: (DataflowConsumerTemplate ct c sc) => ct -> ( c -> DataflowInstr ) -> DataflowInstr
   -- Empuja un chunk al consumidor especificado
   DIPushChunk :: (DataflowConsumer c sc) => c -> sc -> DataflowInstr -> DataflowInstr
   -- Termina el uso del consumidor
   DIDisposeConsumer :: (DataflowConsumer c sc) => c -> DataflowInstr -> DataflowInstr

   -- Crea un transformador con el template especificado. Devuelve el transformador.
   DINewTransformer :: (DataflowTransformerTemplate tt t isc osc) => tt -> ( t -> DataflowInstr ) -> DataflowInstr
   -- Realiza una transformación con el transformador y el chunk de entrada especificado. Devuelve el chunk transformado.
   DITransform :: (DataflowTransformer t isc osc) => t -> isc -> (osc -> DataflowInstr) -> DataflowInstr
   -- Termina el uso del transformador especificado
   DIDisposeTransformer :: (DataflowTransformer t isc osc) => t -> DataflowInstr

   -- Une dos chunks en una tupla
   DIJoin :: (DataflowChunkTuple tsc lsc rsc) => lsc -> rsc -> ( tsc -> DataflowInstr ) -> DataflowInstr
   -- Divide la tupla de chunks, separándola en sus componentes
   DISplit :: (DataflowChunkTuple tsc lsc rsc) => tsc -> ( (lsc, rsc) -> DataflowInstr ) -> DataflowInstr
   -- Devuelve una porción del chunk con el offset y el tamaño especificados
   DISection :: (DataflowChunk sc) => sc -> Word64 -> Word64 -> ( sc -> DataflowInstr ) -> DataflowInstr
   -- Termina el uso del chunk
   DIDisposeChunk :: (DataflowChunk sc) => sc -> DataflowInstr
   
   -- Termina el programa
   DIEnd :: DataflowInstr
