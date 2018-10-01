{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFramework.Signal.Operations.Transformer(
     SFTransformer(SFTransformer, sftFirstState)
   , SFTransformerTickCmd(
          SFTransformerIOTickCmd
        , SFTransformerInplaceTickCmd
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
import Data.Functor
import Control.Exception
import Control.Monad
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
   Comando de transformación de chunk
-}
data SFTransformerTickCmd sc = SFTransformerIOTickCmd ( SFSignalChunkIO sc ) | SFTransformerInplaceTickCmd sc

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
           la acción AsyncIO de recepción de futuro de comando de tick a realizar,
           y la función que construye una acción a partir de la acción que
           realiza el tick.
           Ésta devuelve una acción que espera la compleción del tick, y
           otra acción que representa la continuación del transformador.
           Dependiendo de la implementación, la continuación puede bloquear.
           Con éstos dos elementos construye una acción compuesta.

           Devuelve el próximo estado.
        -}
      , sftPushTickOp :: (SFSignalChunk sc) => Word64 -> AsyncIO ( Future ( SFTransformerTickCmd sc ) ) -> ( AsyncIO ( Future (), AsyncIO () ) -> AsyncIO () ) -> SFTransformerSt sc
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
           la acción AsyncIO de elaboración del comando de tick,
           y la función que elabora la operación de tick desde el cliente.
           Ésta devuelve otra acción que representa la continuación del transformador.

           Devuelve el próximo estado.
        -}
      , sftTvPushTickOp :: (SFSignalChunk sc) => Word64 -> AsyncIO ( SFTransformerTickCmd sc ) -> ( AsyncIO ( AsyncIO () ) -> AsyncIO () ) -> ( SFTVTransformerSt sc)
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
                , sftPushTickOp = \chunkLength dstMkCmd dstMkClientOp ->
                     let srcMkCmd = dstMkCmd >>= await
                         srcMkClientOp = \srcDoOp ->
                            dstMkClientOp $ do
                               srcDoOpFuture <- async srcDoOp
                               
                               return ( void srcDoOpFuture, join $ await srcDoOpFuture )

                     in mkSFTransformerSt $ srcPushTickOp chunkLength srcMkCmd srcMkClientOp
                , sftDoPendingOps = liftM mkSFTransformerSt srcDoPendingOps
                , sftTerminate = srcTerminate
                }
   mkSFTransformerSt (SFTVTerminatedTDTransformerSt pendingActions) = SFTerminatedTransformerSt $ pendingActions
