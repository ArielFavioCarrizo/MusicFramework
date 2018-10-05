{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFramework.Signal.Operations.Transformer(
     SFTransformer(SFTransformer, sftNewInstance)
   , SFTransformerTickCmd(
          SFTransformerIOTickCmd
        , SFTransformerInplaceTickCmd
        )
   , SFTransformerSt(
          SFTransformerSt
        , sftMaxFrames
        , sftPushTickOp
        , sftTerminate
        )
   , SFTransformerStConvertible(mkSFTransformerSt)
   , SFTVTransformerSt(
          SFTVTransformerSt
        , sftTvMaxFrames
        , sftTvPushTickOp
        , sftTvTerminate
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
data SFTransformer sc = SFTransformer { sftNewInstance :: AsyncIO ( Maybe ( SFTransformerSt sc ) ) }

{-
   Comando de transformación de chunk
-}
data SFTransformerTickCmd sc = SFTransformerIOTickCmd ( SFSignalChunkIO sc ) | SFTransformerInplaceTickCmd sc

class (SFSignalChunk sc) => SFTransformerStConvertible sc state | state -> sc where
   mkSFTransformerSt :: state -> SFTransformerSt sc -- Convierte a estado de transformador genérico

-- Descripción de estado del transformador stateful
data SFTransformerSt sc = 
   -- Estado de transformador sin terminar
   SFTransformerSt {
        -- Máxima cantidad de frames con los que puede operar en el tick
        sftMaxFrames :: Word64
        {-
           Devuelve una acción querRealiza la operación de tick con el comando de tick a realizar
           especificado y que devuelve el futuro de compleción del tick y el futuro del próximo estado.
        -}
      , sftPushTickOp :: (SFSignalChunk sc) => Future ( SFTransformerTickCmd sc ) -> AsyncIO ( Future (), Future ( Maybe ( SFTransformerSt sc ) ) )
        {-
           Termina el uso del transformador
        -}
      , sftTerminate :: AsyncIO ()
      }

-- Descripción de estado del transformador variante en el tiempo
data SFTVTransformerSt sc = 
   -- Estado de transformador sin terminar
   SFTVTransformerSt {
        -- Máxima cantidad de frames con los que puede operar en el tick
        sftTvMaxFrames :: Word64
        {-
           Devuelve una acción que realiza la operación de tick con el comando especificado, y devuelve el próximo estado.
        -}
      , sftTvPushTickOp :: (SFSignalChunk sc) => SFTransformerTickCmd sc -> AsyncIO ( Maybe ( SFTVTransformerSt sc ) )
        {-
           Termina el uso del transformador
        -}
      , sftTvTerminate :: AsyncIO ()
      }

instance (SFSignalChunk sc) => SFTransformerStConvertible sc (SFTVTransformerSt sc) where
   mkSFTransformerSt
      SFTVTransformerSt {
           sftTvMaxFrames = srcMaxFrames
         , sftTvPushTickOp = srcPushTickOp
         , sftTvTerminate = srcTerminate 
         } =
             SFTransformerSt {
                  sftMaxFrames = srcMaxFrames
                , sftPushTickOp = \cmdFuture -> do
                     nextSrcStateFuture_opt <- async $
                        await cmdFuture >>= srcPushTickOp

                     return ( void nextSrcStateFuture_opt, (liftM mkSFTransformerSt) <$> nextSrcStateFuture_opt )
                , sftTerminate = srcTerminate
                }
