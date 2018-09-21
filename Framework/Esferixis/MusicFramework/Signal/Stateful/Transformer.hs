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
import Control.Exception
import Control.Monad.IO.Class
import Esferixis.MusicFramework.Signal.Stateful.SignalChunk
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise

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
   -- Recibe una lista de funciones que realizan operaciones de ticks. Devuelve el nuevo estado.
   SFTransformerDoStatelessTicksOp ( (SFSignalChunk sc) => [SFTransformerTickOp sc -> IO ()] -> AsyncIO ( Maybe (SFTransformerSt sc) ) ) |
   -- Recibe una función que realiza un tick. Devuelve el nuevo estado
   SFTransformerDoStatefulTickOp ( (SFSignalChunk sc) => ( SFTransformerTickOp sc -> IO () ) -> AsyncIO ( Maybe (SFTransformerSt sc) ) )

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

-- Decora el operador de tick con una acción previa que recibe la longitud
sftTickOpPre :: SFTransformerTickOp sc -> ( Word64 -> IO () ) -> SFTransformerTickOp sc
sftTickOpPre srcTickOp preAction =
   let doOp tickFun input = do
       preAction $ sfscLength input
       tickFun srcTickOp input
   in SFTransformerTickOp {    
        sftTick = doOp sftTick
      , sftTickInplace = doOp sftTickInplace
      }

-- Decora el operador de tick con una acción posterior que recibe la longitud o una excepción en una continuación
sftTickOpPostCont :: SFTransformerTickOp sc -> ( FutureValue Word64 -> IO () ) -> SFTransformerTickOp sc
sftTickOpPostCont srcTickOp postAction =
   let doOp tickFun input = do
       result <- try $ tickFun srcTickOp input
       postAction $ do
          result
          return $ sfscLength input
   in SFTransformerTickOp {    
        sftTick = doOp sftTick
      , sftTickInplace = doOp sftTickInplace
      }

sftTickOpLimit :: SFTransformerTickOp sc -> Maybe Word64 -> SFTransformerTickOp sc
sftTickOpLimit srcTickOp (Just maxFrames) = sftTickOpPre srcTickOp $ \chunkSize ->
   if ( chunkSize > maxFrames )
      then fail "Chunk size is larger than expected"
      else return ()
sftTickOpLimit srcTickOp Nothing = srcTickOp

-- Descripción de estado de transformador stateful que realiza determinadas acciones en instantes de tiempo puntuales
data SFTransformerPActionsSt sc = SFTransformerPActionsSt {
     -- Máxima cantidad de frames tolerados antes de hacer la operación. Si es Nothing significa que es ilimitado.
     sftpaMaxFrames :: Maybe Word64
     -- Operación de transformado de frames
   , sftpaTickOp :: SFTransformerTickOp sc
     -- Pasaje a próximo estado
   , sftpaNextState :: IO ( Maybe ( SFTransformerPActionsSt sc ) )
     -- Destruye el transformador
   , sftpaDelete :: IO ()
   }

-- Crea un estado de transformador a partir de uno basado en acciones puntuales
mkSfTransformerStFromPActionsSt :: Maybe (SFTransformerPActionsSt sc) -> Maybe (SFTransformerSt sc)
mkSfTransformerStFromPActionsSt = ( >>= \srcTransformerSt -> return $
   let maxFramesOpt = sftpaMaxFrames srcTransformerSt
   in SFTransformerSt {
          sftMaxFrames = maxFramesOpt
        , sftDoTicksOp =
             SFTransformerDoStatefulTickOp $ \doTickFun -> do
                chunkLength <- callCC $ \continuation -> doTickFun $ ( ( sftpaTickOp srcTransformerSt ) `sftTickOpLimit` maxFramesOpt ) `sftTickOpPostCont` continuation
                nextState <- liftIO $
                   case maxFramesOpt of
                      Nothing -> return $ Just srcTransformerSt
                      Just maxFrames ->
                         if ( chunkLength < maxFrames )
                            then return $ Just $ srcTransformerSt { sftpaMaxFrames = Just $ maxFrames - chunkLength }
                            else sftpaNextState srcTransformerSt
                return $ mkSfTransformerStFromPActionsSt nextState
        , sftDelete = sftpaDelete srcTransformerSt
        } )
