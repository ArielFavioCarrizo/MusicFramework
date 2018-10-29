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
        -- Máxima cantidad de frames garantizada con los que puede operar en el tick y subsiguientes.
        sftMaxFrames :: AsyncIO Word64
        {-
           Devuelve una acción que realiza la operación de tick con una función que
           recibe el comando de tick a realizar.
        -}
      , sftPushTickOp :: Future ( SFTransformerTickCmd sc ) -> AsyncIO ()
        {-
           Termina el uso del transformador
        -}
      , sftTerminate :: AsyncIO ()
      }

instance SFSignalUnitSt (SFTransformerSt sc) ( Future ( SFTransformerTickCmd sc ) ) where
   sfsuMaxFrames = sftMaxFrames
   sfsuPushTickOp = sftPushTickOp
   sfsuTerminate = sftTerminate
