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
     sftStatelessTick :: (Monad m, SFSignalChunk m sc) => SignalChunkSection sc -> SignalChunkSection sc -> m ()
     -- Realiza un 'tick', produce una sección de señal en la sección chunk especificada, tomándola como entrada y la sobreescribe con la salida
   , sftStatelessTickInplace :: (Monad m, SFSignalChunk m sc) => SignalChunkSection sc -> m ()
     -- Destruye el transformador
   , sftStatelessDelete :: (Monad m) => m ()
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
        sftStatelessMutationAction :: Maybe ( STTransformerMutationAction m sc )
      , sftStatelessOperations :: STTransformerImpOperations m sc
      } |
   SFTransformerStatefulSt {
        {-
           Realiza un 'tick' con la sección de chunk de entrada especificada, produciendo una sección de señal en la sección de chunk destino especificada
           devolviendo un nuevo estado
        -}
        sftStatefulTick :: (Monad m, SFSignalChunk m sc) => SignalChunkSection sc -> SignalChunkSection sc -> m ( SFTransformerSt m sc )
        {-
           Realiza un 'tick', produce una sección de señal en la sección chunk especificada, tomándola como entrada y la sobreescribe con la salida
           devolviendo un nuevo estado
        -}
      , sftStatefulTickInplace :: (Monad m, SFSignalChunk m sc) => SignalChunkSection sc -> m ( SFTransformerSt m sc )
        -- Destruye el transformador
      , sftStatefulDelete :: (Monad m) => m ()
      }
