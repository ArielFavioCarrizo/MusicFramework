{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Signal.Stateful.Transformer(
     SFTransformerTickOp(
          SFTransformerTickOp
        , sftTick
        , sftTickInplace
        )
   , SFTransformerDoTicksOp(
        SFTransformerDoStatelessTicksOp
      , SFTransformerDoStatefulTickOp
      )
   , SFTransformerSt(
          SFTransformerSt
        , sftMaxFrames
        , sftDoTicksOp
        , sftDelete
        )
   ) where

import Data.Word
import Data.Maybe
import Esferixis.MusicFramework.Signal.Stateful.SignalChunk

{-
   Representación de transformador stateful no manejado
-}
data SFTransformer m sc = SFTransformer { sftNewInstance :: (Monad m) => m ( SFTransformerSt m sc ) }

{-
   Operación de transformación de sección de chunk
-}
data SFTransformerTickOp m sc = SFTransformerTickOp {
     -- Realiza un 'tick' con par de E/S de chunks especificado
     sftTick :: (Monad m, SFSignalChunk sc) => SFSignalChunkIO sc -> m ()
     -- Realiza un 'tick', produce una sección de señal en el chunk especificado, tomándolo como entrada y lo muta destructivamente con la salida
   , sftTickInplace :: (Monad m, SFSignalChunk sc) => sc -> m ()
   }

-- Ejecución de operaciones de transformación de frames
data SFTransformerDoTicksOp m sc =
   -- Recibe una lista de funciones que realizan operaciones de ticks y una función que ejecuta acciones en paralelo. Pasa el nuevo estado en la continuación.
   SFTransformerDoStatelessTicksOp ( (Monad m, SFSignalChunk sc) => [SFTransformerTickOp m sc -> m ()] -> ( [m ()] -> m() ) -> ( SFTransformerSt m sc -> m () ) -> m () ) |
   -- Recibe una función que realiza un tick. Pasa el nuevo estado en la continuación.
   SFTransformerDoStatefulTickOp ( (Monad m, SFSignalChunk sc) => ( SFTransformerTickOp m sc -> m () ) -> ( SFTransformerSt m sc -> m () ) -> m () )

{-
   Representación abstracta de un estado de transformador stateful
   no manejado
   
   ATENCIÓN: Toda operación debe ser realizada en el orden
             especificado.

             Toda operación de estado posterior, tiene que ser posterior
             al estado anterior

             Caso contrario se producirá comportamiento indefinido.
-}
data SFTransformerSt m sc = SFTransformerSt {
     -- Máxima cantidad de frames tolerados antes de hacer la operación. Si es Nothing significa que es ilimitado.
     sftMaxFrames :: Maybe Word64
     -- Operación de transformado de frames
   , sftDoTicksOp :: SFTransformerDoTicksOp m sc
     -- Destruye el transformador
   , sftDelete :: (Monad m) => m ()
   }

sftTickOpPre :: (Monad m) => SFTransformerTickOp m sc -> ( Word64 -> m () ) -> SFTransformerTickOp m sc
sftTickOpPre srcTickOp preAction = SFTransformerTickOp {
     sftTick = \ioChunkPair -> do
        preAction $ sfscIOLength ioChunkPair
        sftTick srcTickOp ioChunkPair
   , sftTickInplace = \chunk -> do
        preAction $ sfscLength chunk
        sftTickInplace srcTickOp chunk
   }

sftTickOpPost :: (Monad m) => SFTransformerTickOp m sc -> ( Word64 -> m () ) -> SFTransformerTickOp m sc
sftTickOpPost srcTickOp postAction = SFTransformerTickOp {
     sftTick = \ioChunkPair -> do
        sftTick srcTickOp ioChunkPair
        postAction $ sfscIOLength ioChunkPair
   , sftTickInplace = \chunk -> do
        sftTickInplace srcTickOp chunk
        postAction $ sfscLength chunk
   }

-- Descripción de estado de transformador stateful que realiza determinadas acciones en instantes de tiempo puntuales
data SFTransformerPActionsSt m sc = SFTransformerPActionsSt {
     sftpaFramesToDoAction :: Maybe Word64
   , sftpaTickOp :: (Monad m) => SFTransformerTickOp m sc
   , sftpaNextState :: (Monad m) => Maybe ( SFTransformerPActionsSt m sc )
   , sftpaDelete :: (Monad m) => m ()
   }

mkSfTransformerStFromPActionsSt :: SFTransformerPActionsSt m sc -> SFTransformerSt m sc
mkSfTransformerStFromPActionsSt srcTransformerSt =
   let framesToDoAction = sftpaFramesToDoAction srcTransformerSt
   in SFTransformerSt {
          sftMaxFrames = framesToDoAction
        , sftDoTicksOp =
             SFTransformerDoStatefulTickOp $ \doTickFun continuation ->
                doTickFun $ sftTickOpPost ( sftpaTickOp srcTransformerSt ) $ \chunkLength ->
                   case framesToDoAction of
                      Nothing -> continuation $ mkSfTransformerStFromPActionsSt srcTransformerSt
                      -- FIXME: Completar
        , sftDelete = sftpaDelete srcTransformerSt
        }
