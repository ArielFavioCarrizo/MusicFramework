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

{-
   Operación de transformación de sección de chunk
-}
data SFTransformerTickOp m sc = SFTransformerTickOp {
     -- Realiza un 'tick' con el chunk de entrada especificado, produciendo una sección de señal en el chunk destino especificado.
     sftTick :: (Monad m) => sc -> sc -> m ()
     -- Realiza un 'tick', produce una sección de señal en el chunk especificado, tomándolo como entrada y lo muta destructivamente con la salida
   , sftTickInplace :: (Monad m) => sc -> m ()
   }

-- Ejecución de operaciones de transformación de frames
data SFTransformerDoTicksOp m sc =
   -- Recibe una lista de funciones que realizan operaciones de ticks y una función que ejecuta acciones en paralelo. Produce un nuevo estado.
   SFTransformerDoStatelessTicksOp ( (Monad m) => [SFTransformerTickOp m sc -> m ()] -> ( [m ()] -> m() ) -> m ( SFTransformerSt m sc ) ) |
   -- Recibe una función que realiza un tick. Produce un nuevo estado.
   SFTransformerDoStatefulTickOp ( (Monad m) => ( SFTransformerTickOp m sc -> m () ) -> m ( SFTransformerSt m sc ) )

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
