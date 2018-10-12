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

class SFTransformerStConvertible sc state | state -> sc where
   mkSFTransformerSt :: state -> SFTransformerSt sc -- Convierte a estado de transformador genérico

-- Descripción de estado del transformador stateful
data SFTransformerSt sc = 
   -- Estado de transformador sin terminar
   SFTransformerSt {
        -- Máxima cantidad de frames con los que puede operar en el tick. Los restantes son la cota superior mínima garantizada.
        sftMaxFrames :: Word64
        {-
           Devuelve una acción que realiza la operación de tick con una función que
           recibe el comando de tick a realizar y que devuelve del próximo estado.
        -}
      , sftPushTickOp :: Future ( SFTransformerTickCmd sc ) -> AsyncIO ( Maybe ( SFTransformerSt sc ) )
        {-
           Termina el uso del transformador
        -}
      , sftTerminate :: AsyncIO ()
      }

instance SFSignalUnitSt (SFTransformerSt sc) ( Future ( SFTransformerTickCmd sc ) ) where
   sfsuMaxFrames = sftMaxFrames
   sfsuPushTickOp = sftPushTickOp
   sfsuTerminate = sftTerminate

-- Descripción de estado del transformador variante en el tiempo
data SFTVTransformerSt sc = 
   -- Estado de transformador sin terminar
   SFTVTransformerSt {
        -- Máxima cantidad de frames con los que puede operar en el tick. Los restantes son la cota superior mínima garantizada.
        sftTvMaxFrames :: Word64
        {-
           Devuelve una acción que realiza la operación de tick con el comando especificado, y devuelve el próximo estado.
        -}
      , sftTvPushTickOp :: SFTransformerTickCmd sc -> AsyncIO ( Maybe ( SFTVTransformerSt sc ) )
        {-
           Termina el uso del transformador
        -}
      , sftTvTerminate :: AsyncIO ()
      }

instance SFSignalUnitSt (SFTVTransformerSt sc) ( SFTransformerTickCmd sc ) where
   sfsuMaxFrames = sftTvMaxFrames
   sfsuPushTickOp = sftTvPushTickOp
   sfsuTerminate = sftTvTerminate

instance SFTransformerStConvertible sc (SFTVTransformerSt sc) where
   mkSFTransformerSt
      SFTVTransformerSt {
           sftTvMaxFrames = srcMaxFrames
         , sftTvPushTickOp = srcPushTickOp
         , sftTvTerminate = srcTerminate
         } =
             SFTransformerSt {
                  sftMaxFrames = srcMaxFrames
                , sftPushTickOp = \cmdFuture -> do
                     nextSrcSt <- await cmdFuture >>= srcPushTickOp
                     return $ (liftM mkSFTransformerSt) nextSrcSt
                , sftTerminate = srcTerminate
                }
