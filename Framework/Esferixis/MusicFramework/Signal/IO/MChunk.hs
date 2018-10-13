{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Esferixis.MusicFramework.Signal.IO.MChunk(
     UnmanagedChunk(ucChunk, ucDelete)
   , MChunk
   , mkMChunk
   , mChunkConsume
   , mChunkBind
   ) where

import Data.Word
import Data.Maybe
import Data.Functor
import Type.Reflection
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.MusicFramework.Signal.Operations.Signal

data UnmanagedChunk ch = UnmanagedChunk {
     ucChunk :: ch
   , ucDelete :: AsyncIO ()
   }

{-
   Recibe la entrada y una función que permite realizar un trabajo con el chunk.
   Devuelve la salida de la operación.
-}
type ConsumeFun ch s r = s -> (ch -> AsyncIO ()) -> AsyncIO r

data MChunk ch r where
   MkMChunk :: {
         mcInput :: s
       , mcMkChunk :: AsyncIO (UnmanagedChunk ch) 
       , mcInitChunk :: ConsumeFun ch s r
       } -> MChunk sc r
   NextMChunk :: {
         mcHead :: MChunk sc s
       , mcCmd :: MChunkCmd sc s r
       } -> MChunk sc r

data MChunkCmd ch s r = ConsumeMChunkCmd ( ConsumeFun ch s r ) | BindMChunkCmd ( s -> AsyncIO r )

mkMChunk :: s -> AsyncIO (UnmanagedChunk ch) -> ConsumeFun ch s r -> MChunk sc r
mkMChunk input mkChunk initchunk =
   MkMChunk {
        mcInput = input
      , mcMkChunk = mkChunk
      , mcInitChunk = initchunk
      }

mChunkCmd :: MChunk ch s -> MChunkCmd ch s r -> MChunk ch r
mChunkCmd srcMChunk cmd =
   NextMChunk {
        mcHead = srcMChunk
      , mcCmd = cmd
      }

-- Agrega un consumo al chunk
mChunkConsume :: MChunk ch s -> ConsumeFun ch s r -> MChunk ch r
mChunkConsume srcMChunk fun = mChunkCmd srcMChunk $ ConsumeMChunkCmd $ fun

-- Realiza un bind en el contexto del chunk
mChunkBind :: MChunk ch s -> ( s -> AsyncIO r ) -> MChunk ch r
mChunkBind srcMChunk fun = mChunkCmd srcMChunk $ BindMChunkCmd $ fun

-- TODO: Implementar
--mChunkRun :: MChunk sc r -> AsyncIO r
--mChunkRun mChunk =
