{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal.Stateful.Transformer(
     STTransformerMutationAction(sftRemainingFramesToMutate, sftDoMutationAction)
   , SFTransformerTickOp(sftTick, sftTickInplace)
   , SFTransformerSt(
          SFTransformerStatelessSt
        , sftStatelessMutationAction
        , sftStatelessTickOp
        , sftStatelessDelete
        , SFTransformerStatefulSt
        , sftStatefulMaxChunkSecSize
        , sftStatefulTickOp
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
data STTransformerMutationAction m sc opIn = STTransformerMutationAction {
     -- Cantidad de frames para mutar
     sftRemainingFramesToMutate :: Word64
     -- Realiza la acción de mutación que corresponde después de procesar las secciones de chunk correspondientes
   , sftDoMutationAction :: (Monad m) => opIn -> m opIn
     -- Pasa al siguiente estado. Si devuelve Nothing, el transformador no recibe más frames y es destruido en la acción de mutación.
   , sftNextState :: (Monad m, SFSignalChunk m sc ) => Maybe ( SFTransformerSt m sc opIn )
   }

{-
   Operación de transformación de sección de chunk
-}
data SFTransformerTickOp m sc opIn = SFTransformerTickOp {
     -- Realiza un 'tick' con la sección de chunk de entrada especificada, produciendo una sección de señal en la sección de chunk destino especificada
     sftTick :: (Monad m, SFSignalChunk m sc) => opIn -> SignalChunkSection sc -> SignalChunkSection sc -> m opIn
     -- Realiza un 'tick', produce una sección de señal en la sección chunk especificada, tomándola como entrada y la sobreescribe con la salida
   , sftTickInplace :: (Monad m, SFSignalChunk m sc) => opIn -> SignalChunkSection sc -> m opIn
   }

{-
   Crea una nueva acción de transformación de sección de chunk que
   realiza la acción de transformación de chunk especificada después de la acción
   de mutación especificada 
-}
sftTickOpPre :: (Monad m, SFSignalChunk m sc) => (opIn -> m opIn) -> SFTransformerTickOp m sc opIn -> SFTransformerTickOp m sc opIn
sftTickOpPre preAction srcChunkOp = SFTransformerTickOp {
     sftTick = \opInput srcChunkSec dstChunkSec -> do
        opInput2 <- preAction opInput
        sftTick srcChunkOp opInput2 srcChunkSec dstChunkSec
   , sftTickInplace = \opInput chunkSec -> do
        opInput2 <- preAction opInput
        sftTickInplace srcChunkOp opInput2 chunkSec
   }

{-
   Crea una nueva acción de transformación de sección de chunk que
   realiza la acción de transformación de chunk especificada antes de la acción
   de mutación especificada
-}
sftTickOpPost :: (Monad m, SFSignalChunk m sc) => SFTransformerTickOp m sc opIn -> (opIn -> m opIn) -> SFTransformerTickOp m sc opIn
sftTickOpPost srcChunkOp postAction = SFTransformerTickOp {
     sftTick = \opInput srcChunkSec dstChunkSec -> do
        opInput2 <- sftTick srcChunkOp opInput srcChunkSec dstChunkSec
        opInput3 <- postAction opInput2
        return opInput3
   , sftTickInplace = \opInput chunkSec -> do
        opInput2 <- sftTickInplace srcChunkOp opInput chunkSec
        opInput3 <- postAction opInput2
        return opInput3
   }

sftTickOpLift :: (Monad m, SFSignalChunk m sc) => SFTransformerTickOp m sc () -> SFTransformerTickOp m sc opIn
sftTickOpLift srcTickOp = SFTransformerTickOp {
     sftTick = \opInput srcChunkSec dstChunkSec -> do
        sftTick srcTickOp () srcChunkSec dstChunkSec
        return opInput
   , sftTickInplace = \opInput chunkSec -> do
        sftTickInplace srcTickOp () chunkSec
        return opInput
   }

{-
   Representación abstracta de un transformador stateful
   no manejado
   
   ATENCIÓN: Toda operación debe ser realizada en el orden
             especificado.

             Toda operación de estado posterior, tiene que ser posterior
             al estado anterior

             Caso contrario se producirá comportamiento indefinido.
-}
data SFTransformerSt m sc opIn =
   {-
      Estado de transformador stateless
 
      Si hay acción de mutación debe efectuarse después de realizar el 'tick'
      al acabar la cantidad de frames para mutar.
   -}
   SFTransformerStatelessSt {
        -- Si el transformador muta, es la acción de mutación
        sftStatelessMutationAction :: (Monad m, SFSignalChunk m sc) => Maybe ( STTransformerMutationAction m sc opIn )
        -- Acción de transformado de chunk
      , sftStatelessTickOp :: SFTransformerTickOp m sc ()
        -- Destruye el transformador. No tiene que haber pendiente ninguna acción de transformado de chunk.
      , sftStatelessDelete :: m ()
      } |
   {-
      Estado de transformador stateful
   -}
   SFTransformerStatefulSt {
        -- Máximo tamaño de sección de chunk tolerado
        sftStatefulMaxChunkSecSize :: Word64
        {-
           Crea una operación de transformado de chunk con el tamaño de frames
           a procesar especificados.
           Y devuelve el próximo estado.

           Si el estado es Nothing, significa que el transformador será destruido al terminar la transformación de chunk
        -}
      , sftStatefulTickOp :: Word64 -> ( SFTransformerTickOp m sc opIn, Maybe (SFTransformerSt m sc opIn) )
        -- Destruye el transformador.
      , sftStatefulDelete :: (Monad m) => m ()
      }

{-
   Crea un transformador imperativo variante en el tiempo
   con las operaciones externas, y la lista de acciones de mutación.
   Donde cada elemento de la lista es una tupla que lleva la
   acción, y la duración de la vigencia del estado impuesto
   por la acción
-}
data ImpTransformerExtOp m sc = ImpTransformerExtOp {
     -- Operaciones de chunk
     impTransformerTickOp :: SFTransformerTickOp m sc ()
     -- Operación de destrucción
   , impTransformerDelete :: m ()
   }

newTimevariantImpTransformer :: (Monad m, SFSignalChunk m sc) => ImpTransformerExtOp m sc -> [(opIn -> m opIn, Word64)] -> Maybe (SFTransformerSt m sc opIn)
newTimevariantImpTransformer impOperations ((mAction, mActionDuration):remainingActions) =
   let selfNext = newTimevariantImpTransformer impOperations
       deleteOp = impTransformerDelete impOperations
   in Just $ SFTransformerStatefulSt {
           sftStatefulMaxChunkSecSize = mActionDuration
         , sftStatefulTickOp = \chunkLength ->
              let lonelyTickOp = sftTickOpLift (impTransformerTickOp impOperations)

              in case ( compare chunkLength mActionDuration ) of
                    LT ->
                       let tickOp = mAction `sftTickOpPre` lonelyTickOp
                           nextState = selfNext ( ( (\opIn -> return opIn), mActionDuration - chunkLength):remainingActions )
                       in ( tickOp, nextState )
                    EQ ->
                       let nextState = selfNext remainingActions
                           chunkOp = mAction `sftTickOpPre` lonelyTickOp `sftTickOpPost` do
                              case nextState of
                                 Just a -> (\opIn -> return opIn)
                                 Nothing -> \opIn -> do
                                    deleteOp
                                    return opIn

                       in ( chunkOp, nextState )
                    GT -> error "Chunk section length is greater than expected"
         , sftStatefulDelete = deleteOp
         }

newTimevariantImpTransformer impOperations [] = Nothing
