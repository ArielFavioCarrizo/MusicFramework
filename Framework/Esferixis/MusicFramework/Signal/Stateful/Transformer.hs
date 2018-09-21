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
import Esferixis.Control.Concurrency.AsyncIO

{-
   Representación de transformador stateful no manejado
-}
data SFTransformer sc = SFTransformer { sftNewInstance :: IO ( SFTransformerSt sc ) }

{-
   Operación de transformación de sección de chunk
-}
data SFTransformerTickOp sc = SFTransformerTickOp {
     -- Realiza un 'tick' con par de E/S de chunks especificado
     sftTick :: (SFSignalChunk sc) => SFSignalChunkIO sc -> IO ()
     -- Realiza un 'tick', produce una sección de señal en el chunk especificado, tomándolo como entrada y lo muta destructivamente con la salida
   , sftTickInplace :: (SFSignalChunk sc) => sc -> IO ()
   }

-- Ejecución de operaciones de transformación de frames
data SFTransformerDoTicksOp sc =
   {-
      Recibe una lista de funciones que realizan operaciones de ticks y una función que ejecuta acciones en paralelo. Pasa el nuevo estado en la continuación.
   -}
   SFTransformerDoStatelessTicksOp ( (SFSignalChunk sc) => [SFTransformerTickOp sc -> IO ()] -> ( [IO ()] -> AsyncIO () ) -> ( Maybe (SFTransformerSt sc) -> AsyncIO () ) -> AsyncIO () ) |
   -- Recibe una función que realiza un tick. Pasa el nuevo estado en la continuación.
   SFTransformerDoStatefulTickOp ( (SFSignalChunk sc) => ( SFTransformerTickOp sc -> IO () ) -> ( Maybe (SFTransformerSt sc) -> IO () ) -> IO () )

{-
   Representación abstracta de un estado de transformador stateful
   no manejado
   
   ATENCIÓN: Toda operación debe ser realizada en el orden
             especificado.

             Toda operación de estado posterior, tiene que ser posterior
             al estado anterior

             Caso contrario se producirá comportamiento indefinido.
-}
data SFTransformerSt sc = SFTransformerSt {
     -- Máxima cantidad de frames tolerados antes de hacer la operación. Si es Nothing significa que es ilimitado.
     sftMaxFrames :: Maybe Word64
     -- Operación de transformado de frames
   , sftDoTicksOp :: SFTransformerDoTicksOp sc
     -- Destruye el transformador
   , sftDelete :: IO ()
   }

sftTickOpPre :: SFTransformerTickOp sc -> ( Word64 -> IO () ) -> SFTransformerTickOp sc
sftTickOpPre srcTickOp preAction = SFTransformerTickOp {
     sftTick = \ioChunkPair -> do
        preAction $ sfscIOLength ioChunkPair
        sftTick srcTickOp ioChunkPair
   , sftTickInplace = \chunk -> do
        preAction $ sfscLength chunk
        sftTickInplace srcTickOp chunk
   }

sftTickOpPost :: SFTransformerTickOp sc -> ( Word64 -> IO () ) -> SFTransformerTickOp sc
sftTickOpPost srcTickOp postAction = SFTransformerTickOp {
     sftTick = \ioChunkPair -> do
        sftTick srcTickOp ioChunkPair
        postAction $ sfscIOLength ioChunkPair
   , sftTickInplace = \chunk -> do
        sftTickInplace srcTickOp chunk
        postAction $ sfscLength chunk
   }

-- Descripción de estado de transformador stateful que realiza determinadas acciones en instantes de tiempo puntuales
data SFTransformerPActionsSt sc = SFTransformerPActionsSt {
     sftpaFramesToDoAction :: Maybe Word64
   , sftpaTickOp :: SFTransformerTickOp sc
   , sftpaNextState :: Maybe ( SFTransformerPActionsSt sc )
   , sftpaDelete :: IO ()
   }

mkSfTransformerStFromPActionsSt :: Maybe (SFTransformerPActionsSt sc) -> Maybe (SFTransformerSt sc)
mkSfTransformerStFromPActionsSt (Just srcTransformerSt) =
   let framesToDoActionOpt = sftpaFramesToDoAction srcTransformerSt
   in Just $ SFTransformerSt {
          sftMaxFrames = framesToDoActionOpt
        , sftDoTicksOp =
             SFTransformerDoStatefulTickOp $ \doTickFun continuation ->
                doTickFun $ sftTickOpPost ( sftpaTickOp srcTransformerSt ) $ \chunkLength ->
                   continuation $ mkSfTransformerStFromPActionsSt $
                      case framesToDoActionOpt of
                         Nothing -> Just srcTransformerSt
                         Just framesToDoAction ->
                            if ( chunkLength < framesToDoAction )
                               then Just $ srcTransformerSt { sftpaFramesToDoAction = ( Just ( framesToDoAction - chunkLength ) ) }
                               else sftpaNextState srcTransformerSt
        , sftDelete = sftpaDelete srcTransformerSt
        }
