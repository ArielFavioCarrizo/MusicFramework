{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Esferixis.MusicFramework.Signal.IO.MChunk(
     MChunk
   , mkMChunk
   , mcSplitRef
   , mcConsume
   ) where

import Data.Word
import Data.Maybe
import Data.Functor
import Control.Monad.IO.Class
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.MusicFramework.Signal.Operations.Signal
import Esferixis.MusicFramework.Signal.IO.FramesChunk
import Esferixis.Control.Concurrency.RefCount

data MChunk ch = MChunk {
     mcChunk :: ch
   , mcRefCount :: RefCount
   }

mkMChunk :: (SFFramesChunk fc) => IO fc -> AsyncIO (MChunk fc)
mkMChunk mkFramesChunk = do
   chunk <- liftIO $ mkFramesChunk
   refCount <- mkRefCount $ liftIO $ sffcDelete chunk

   return
      MChunk {
           mcChunk = chunk
         , mcRefCount = refCount
         }

mcSplitRef :: MChunk ch -> AsyncIO ( MChunk ch, MChunk ch )
mcSplitRef srcMChunk = do
   refCountIncRef $ mcRefCount srcMChunk
   return ( srcMChunk, srcMChunk )

mcTransform :: MChunk sch -> ( sch -> dch ) -> MChunk dch
mcTransform srcMChunk fun =
   srcMChunk { mcChunk = fun $ mcChunk srcMChunk }

mcConsume :: MChunk ch -> ( ch -> AsyncIO () ) -> AsyncIO ()
mcConsume mchunk fun = do
   fun $ mcChunk mchunk
   refCountDecRef $ mcRefCount mchunk
