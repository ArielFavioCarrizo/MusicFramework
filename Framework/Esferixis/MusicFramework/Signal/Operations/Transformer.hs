{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFramework.Signal.Operations.Transformer(
     SFTransformer(SFTransformer, sftFirstState)
   , SFTransformerTickOp(
          SFTransformerPureTickOp
        , SFTransformerInplaceTickOp
        )
   , SFTransformerSt(
          SFReadyTransformerSt
        , sftMaxFrames
        , sftPushTickOp
        , sftDoPendingOps
        , sftTerminate
        , SFTerminatedTransformerSt
        )
   , SFTransformerStConvertible(mkSFTransformerSt)
   , SFTVTransformerSt(
          SFTVReadyTransformerSt
        , sftTvMaxFrames
        , sftTvPushTickOp
        , sftTvDoPendingOps
        , sftTvTerminate
        , SFTVTerminatedTDTransformerSt
        )
   ) where

import Data.Word
import Data.Maybe
import Control.Exception
import Control.Monad.IO.Class
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.MusicFramework.Signal.Misc
import Esferixis.MusicFramework.Signal.Operations.Signal

{-
   Representación de transformador puro con operaciones stateful
-}
data SFTransformer sc = SFTransformer { sftFirstState :: SFTransformerSt sc }

{-
   Operación de transformación de chunk
-}
data SFTransformerTickOp sc = SFTransformerPureTickOp ( SFSignalChunkIO sc ) | SFTransformerInplaceTickOp sc

class (SFSignalChunk sc) => SFTransformerStConvertible sc state | state -> sc where
   mkSFTransformerSt :: state -> SFTransformerSt sc -- Convierte a estado de transformador genérico

-- Descripción de estado del transformador puro, con operaciones stateful
data SFTransformerSt sc = 
   -- Estado de transformador sin terminar
   SFReadyTransformerSt {
        -- Máxima cantidad de frames con los que puede operar en el tick
        sftMaxFrames :: Word64
        {-
           Agrega la operación de tick con el tamaño de chunk,
           la acción AsyncIO de recepción de operación de tick,
           y la función que elabora la operación de tick desde el cliente,
           con la acción de realización del tick.

           Devuelve el próximo estado.
         -}
      , sftPushTickOp :: (SFSignalChunk sc) => Word64 -> AsyncIO ( Future ( SFTransformerTickOp sc ) ) -> ( AsyncIO ( Future () ) -> AsyncIO () ) -> SFTransformerSt sc
        -- Realiza las acciones pendientes y devuelve el próximo estado
      , sftDoPendingOps :: AsyncIO ( SFTransformerSt sc )
        {-
           Termina el uso del transformador, devolviendo
           una acción que realiza todas las operaciones pendientes
         -}
      , sftTerminate :: AsyncIO ()
      } |
   -- Estado de transformador terminado. Contiene la acción asíncrona que realiza todas las acciones pendientes.
   SFTerminatedTransformerSt ( AsyncIO () )

-- Descripción de estado del transformador variante en el tiempo
data SFTVTransformerSt sc = 
   -- Estado de transformador sin terminar
   SFTVReadyTransformerSt {
        -- Máxima cantidad de frames con los que puede operar en el tick
        sftTvMaxFrames :: Word64
        {-
           Agrega la operación de tick con el tamaño de chunk,
           la acción AsyncIO de recepción de operación de tick,
           y la función que elabora la operación de tick desde el cliente,
           con la acción de realización del tick.

           Devuelve el próximo estado.
         -}
      , sftTvPushTickOp :: (SFSignalChunk sc) => Word64 -> AsyncIO ( SFTransformerTickOp sc ) -> ( AsyncIO () -> AsyncIO () ) -> ( SFTVTransformerSt sc)
        -- Realiza las acciones pendientes y devuelve el próximo estado
      , sftTvDoPendingOps :: AsyncIO ( SFTVTransformerSt sc )
        {-
           Termina el uso del transformador, devolviendo
           una acción que realiza todas las operaciones pendientes
         -}
      , sftTvTerminate :: AsyncIO ()
      } |
   -- Estado de transformador terminado. Contiene la acción AsyncIO que realiza todas las acciones pendientes.
   SFTVTerminatedTDTransformerSt ( AsyncIO () )

instance (SFSignalChunk sc) => SFTransformerStConvertible sc (SFTVTransformerSt sc) where
   mkSFTransformerSt
      SFTVReadyTransformerSt {
           sftTvMaxFrames = srcMaxFrames
         , sftTvPushTickOp = srcPushTickOp
         , sftTvDoPendingOps = srcDoPendingOps
         , sftTvTerminate = srcTerminate 
         } =
             SFReadyTransformerSt {
                  sftMaxFrames = srcMaxFrames
                , sftPushTickOp = \chunkLength dstDoPrevOp dstMkClientOp ->
                     let srcDoPrevOp = dstDoPrevOp >>= await
                         srcMkClientOp = \srcDoOp -> do
                            srcDoOpFuture <- async srcDoOp
                            dstMkClientOp $ return srcDoOpFuture
                            await srcDoOpFuture

                     in mkSFTransformerSt $ srcPushTickOp chunkLength srcDoPrevOp srcMkClientOp
                , sftDoPendingOps = do
                     srcNewState <- srcDoPendingOps
                     return $ mkSFTransformerSt $ srcNewState
                , sftTerminate = srcTerminate
                }
   mkSFTransformerSt (SFTVTerminatedTDTransformerSt pendingActions) = SFTerminatedTransformerSt $ pendingActions
