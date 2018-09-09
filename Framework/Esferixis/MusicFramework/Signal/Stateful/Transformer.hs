{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal.Stateful.Transformer
   ( SFTransformerSt() ) where

import Data.Word
import Data.Maybe

import Esferixis.MusicFramework.Signal.Stateful.SignalChunk

{-
   Operación de mutación del transformador
-}
data STTransformerMutationAction m sc = STTransformerMutationAction {
     sftRemainingFramesToMutate :: Word64 -- Cantidad de frames para mutar
   , sftDoMutationAction :: (Monad m, SFSignalChunk m sc ) => m ( Maybe ( SFTransformerSt m sc ) ) -- Operación de mutación al terminar de transformar la cantidad de frames especificada anteriormente. Si devuelve Nothing, el transformador no recibe más frames y es destruido.
   }

{-
   Representación abstracta de un transformador stateful
   no manejado

   Si hay acción de mutación debe efectuarse después de realizar el 'tick'
   al acabar la cantidad de frames para mutar.
   
   Si la cantidad de frames para mutar es cero, tiene que realizarse el tick
   y después realizar la mutación
-}
data SFTransformerSt m sc = UnmanagedProducerSt {
     sftMutationAction :: Maybe ( STTransformerMutationAction m sc ) -- Si el transformador muta, es la acción de mutación
   , sftTick :: (Monad m, SFSignalChunk m sc) => SignalChunkSection sc -> SignalChunkSection sc -> m () -- Realiza un 'tick' con la sección de chunk de entrada especificada, produciendo una sección de señal en la sección de chunk destino especificada
   , sftTickInplace :: (Monad m, SFSignalChunk m sc) => SignalChunkSection sc -> m () -- Realiza un 'tick', produce una sección de señal en la sección chunk especificada, tomándola como entrada y la sobreescribe con la salida
   , sftDelete :: (Monad m) => m () -- Destruye el transformador
   }
