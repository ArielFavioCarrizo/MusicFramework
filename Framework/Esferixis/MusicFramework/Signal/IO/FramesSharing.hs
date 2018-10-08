{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Esferixis.MusicFramework.Signal.IO.FramesSharing(
     sharedSSConsume
   , sharedSSLength
   ) where

import Data.Word
import Data.Maybe
import Control.Exception
import Control.Monad.IO.Class
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.MusicFramework.Signal.IO.FramesChunk

data SharedSignalChunk sc = SharedSignalChunk {
     sharedSSSrc :: sc
   , sharedSSRemoveRef :: AsyncIO ()
   , sharedSSLength :: Word64
   }

sharedSSConsume :: SharedSignalChunk sc -> ( sc -> AsyncIO r ) -> AsyncIO r
sharedSSConsume sharedSS fun = do
   result :: Either SomeException r <- (tryAsyncIO $ fun $ sharedSSSrc sharedSS)
   sharedSSRemoveRef sharedSS
   case result of
      Right value -> return value
      Left e -> throwAsyncIO e
