{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Signal.Operations.Transformer(
     SFTransformer(SFTransformer, sftNewInstance)
   , SFTransformerTickOp(
          SFTransformerTickOp
        , sftTick
        , sftTickInplace
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
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.MusicFramework.Signal.Misc
import Esferixis.MusicFramework.Signal.Operations.Signal

{-
   Representación de transformador stateful no manejado
-}
data SFTransformer sc = SFTransformer { sftNewInstance :: AsyncIO ( Maybe ( SFTransformerSt sc ) ) }

{-
   Operación de transformación de sección de chunk
-}
data SFTransformerTickOp sc = SFTransformerTickOp {
     -- Realiza un 'tick' con par de E/S de chunks especificado
     sftTick :: (SFSignalChunk sc) => SFSignalChunkIO sc -> IO ()
     -- Realiza un 'tick', produce una sección de señal en el chunk especificado, tomándolo como entrada y lo muta destructivamente con la salida
   , sftTickInplace :: (SFSignalChunk sc) => sc -> IO ()
   }

data SFTransformerSt sc = SFTransformerSt {
     -- Máxima cantidad de frames con los que puede operar en el en el tick
     sftMaxFrames :: Word64
     {-
        Operación de transformado de frames
        Si la transformación de señal termina devuelve Nothing
        y se destruye el transformador.
       
        Devuelve el futuro del resultado de la operación y el próximo estado.
     -}
   , sftDoTicksOp :: (SFSignalChunk sc) => ( SFTransformerTickOp sc -> IO () ) -> AsyncIO ( Maybe ( Future (), SFTransformerSt sc ) )
     -- Destruye el transformador
   , sftDelete :: AsyncIO ()
   }

