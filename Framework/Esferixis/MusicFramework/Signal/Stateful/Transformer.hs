{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal.Stateful.Transformer(
     STTransformerMutationAction(sftRemainingFramesToMutate, sftDoMutationAction)
   , SFTransformerSt(
          SFTransformerStatelessSt
        , sftStatelessMutationAction
        , sftStatelessOperations
        , SFTransformerStatefulSt
        , sftStatefulTick
        , sftStatefulTickInplace
        , sftStatefulDelete
        )
   , newTimevariantImpTransformer
   ) where

import Data.Word
import Data.Maybe

import Esferixis.MusicFramework.Signal.Stateful.SignalChunk

{-
   Operación de mutación del transformador
-}
data STTransformerMutationAction m sc = STTransformerMutationAction {
     -- Cantidad de frames para mutar
     sftRemainingFramesToMutate :: Word64
     -- Operación de mutación al terminar de transformar la cantidad de frames especificada anteriormente. Si devuelve Nothing, el transformador no recibe más frames y es destruido.
   , sftDoMutationAction :: (Monad m, SFSignalChunk m sc ) => m ( Maybe ( SFTransformerSt m sc ) )
   }

{-
   Operaciones imperativas del transformador
-}
data STTransformerImpOperations m sc = STTransformerImpOperations {
     -- Realiza un 'tick' con la sección de chunk de entrada especificada, produciendo una sección de señal en la sección de chunk destino especificada
     sftTick :: (Monad m, SFSignalChunk m sc) => SignalChunkSection sc -> SignalChunkSection sc -> m ()
     -- Realiza un 'tick', produce una sección de señal en la sección chunk especificada, tomándola como entrada y la sobreescribe con la salida
   , sftTickInplace :: (Monad m, SFSignalChunk m sc) => SignalChunkSection sc -> m ()
     -- Destruye el transformador
   , sftDelete :: (Monad m) => m ()
   }

{-
   Representación abstracta de un transformador stateful
   no manejado
-}
data SFTransformerSt m sc =
   {-
      Si hay acción de mutación debe efectuarse después de realizar el 'tick'
      al acabar la cantidad de frames para mutar.
   
      Si la cantidad de frames para mutar es cero, tiene que realizarse el tick
      y después realizar la mutación
   -}
   SFTransformerStatelessSt {
        -- Si el transformador muta, es la acción de mutación
        sftStatelessMutationAction :: (Monad m, SFSignalChunk m sc) => Maybe ( STTransformerMutationAction m sc )
      , sftStatelessOperations :: (Monad m, SFSignalChunk m sc) => STTransformerImpOperations m sc
      } |
   SFTransformerStatefulSt {
        -- Máximo tamaño de sección de chunk tolerado
        sftStatefulMaxChunkSecSize :: Word64
        {-
           Realiza un 'tick' con la sección de chunk de entrada especificada, produciendo una sección de señal en la sección de chunk destino especificada
           devolviendo un nuevo estado
        -}
      , sftStatefulTick :: (Monad m, SFSignalChunk m sc) => SignalChunkSection sc -> SignalChunkSection sc -> m ( Maybe (SFTransformerSt m sc) )
        {-
           Realiza un 'tick', produce una sección de señal en la sección chunk especificada, tomándola como entrada y la sobreescribe con la salida
           devolviendo un nuevo estado
        -}
      , sftStatefulTickInplace :: (Monad m, SFSignalChunk m sc) => SignalChunkSection sc -> m ( Maybe (SFTransformerSt m sc) )
        -- Destruye el transformador
      , sftStatefulDelete :: (Monad m) => m ()
      }

{-
   Crea un transformador imperativo variante en el tiempo
   con las operaciones externas, y la lista de acciones de mutación.
   Donde cada elemento de la lista es una tupla que lleva la
   acción, y la duración de la vigencia del estado impuesto
   por la acción
-}
newTimevariantImpTransformer :: (Monad m, SFSignalChunk m sc) => STTransformerImpOperations m sc -> [(m (), Word64)] -> Maybe (SFTransformerSt m sc)
newTimevariantImpTransformer impOperations ((mAction, mActionDuration):remainingActions) =
   let selfNext = newTimevariantImpTransformer impOperations
       nextState chunkLength =
          if ( chunkLength < mActionDuration )
             then return (selfNext ( (mAction, mActionDuration - chunkLength):remainingActions) )
             else do
                mAction
                return (selfNext remainingActions)

   in Just $ SFTransformerStatefulSt {
           sftStatefulMaxChunkSecSize = mActionDuration
         , sftStatefulTick = \srcChunkSec dstChunkSec -> do
              sftTick impOperations srcChunkSec dstChunkSec
              nextState ( scsLength srcChunkSec )
         , sftStatefulTickInplace = \chunkSec -> do
              sftTickInplace impOperations chunkSec
              nextState ( scsLength chunkSec )
         , sftStatefulDelete = sftDelete impOperations
         }

newTimevariantImpTransformer impOperations [] = Nothing
