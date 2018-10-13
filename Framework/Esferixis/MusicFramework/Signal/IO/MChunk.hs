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
import Esferixis.Control.Concurrency.RefCounted

data MChunk ch = MChunk {
     mcChunk :: ch
   , mcSplitRef :: AsyncIO ( ch, ch )
   , mcTransform :: MChunk ch -> MChunk fc
   , mcDeleteRef :: AsyncIO ()
   }

mkMChunk :: AsyncIO (SFFramesChunk fc) -> MChunk ch
mkMChunk mkFramesChunk = do
   framesChunk <- mkFramesChunk
   rfFramesChunk < mkRefCounted framesChunk $ sffcDelete framesChunk
   return $
      MChunk (SFFramesChunk fc)
           mcChunk = framesChunk
         , mcSplitRef = 
