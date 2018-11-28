{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Esferixis.MusicFramework.Signal.IO.DSPEngine(
     ) where

import Data.Word
import Data.Maybe
import Data.Proxy
import Control.Applicative
import Control.Monad
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.Control.Concurrency.RefCount
import Esferixis.MusicFramework.Signal.IO.MSignalIO
import Esferixis.MusicFramework.Signal.Operations.Signal

class DSPEnvironment e fc sc | e -> fc sc where
   dspFrameChunkLength :: Proxy e -> fc -> Word64
   dspFrameChunkChannels :: Proxy e -> fc -> Word32
   dspFrameChunkDelete :: Proxy e -> fc -> AsyncIO ()

data DSPFrameChunkObj e fc sc = DSPFrameChunkObj {
     unwrappedDspFrameChunk :: (DSPEnvironment e fc sc) => fc
   , dspFcOperativeRefCount :: RefCount
   , dspFcDSLRefCount :: RefCount
   }

data DSPSignalChunkObj e fc sc = DSPSignalChunkObj {
     dspScSource :: DSPFrameChunkObj e fc sc
   }

instance (DSPEnvironment e fc sc) => MFramesChunk e (DSPFrameChunkObj e fc sc) (DSPSignalChunkObj e fc sc)
