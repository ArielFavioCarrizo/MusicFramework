{-# LANGUAGE Rank2Types #-}

module Esferixis.Mutable.Promise where

import Data.Word
import Data.Maybe
import Control.Concurrent.Lock
import Data.Mutable
import Data.IORef

data Promise a =
   Promise {
        pPendingActions :: BDeque RealWorld (IO ())
      , pValue :: IORef (Maybe (IO a))
      , pLock :: Lock
      }

data Future a = Future (Promise a)
fGetPromise (Future promise) = promise

newPromise :: IO (Promise a)
newPromise = do
   pendingActions <- newColl
   value <- newIORef Nothing
   lock <- new
   return ( Promise {
        pPendingActions = pendingActions
      , pValue = value
      , pLock = lock
      } )
