{-# LANGUAGE PartialTypeSignatures #-}

module Esferixis.Mutable.Promise(Promise, Future, newPromise, pSet, future, fGet) where

import Data.Word
import Data.Maybe
import Control.Concurrent.Lock
import Data.Mutable
import Data.IORef
import Control.Exception

data Promise a =
   Promise {
        pNotifyActions :: BDeque RealWorld (IO a -> IO())
      , pValue :: IORef (Maybe (IO a))
      , pLock :: Lock
      }

data Future a = Future (Promise a)

newPromise :: IO (Promise a)
newPromise = do
   notifyActions <- newColl
   valueRef <- newIORef Nothing
   lock <- new
   return ( Promise {
        pNotifyActions = notifyActions
      , pValue = valueRef
      , pLock = lock
      } )

pSet :: Promise a -> IO a -> IO ()
pSet promise ioaction = do
   result <- (try ioaction) :: IO (Either SomeException _)
   let getAction = 
          case result of
             Left e -> throwIO e
             Right value -> return value

   with (pLock promise) $ do
      let valueRef = pValue promise
      value <- readIORef valueRef
      case value of
         Nothing -> writeIORef valueRef (Just getAction)
         Just oldValue -> fail "Cannot set value when it has been set"

   pNotifyValue (pNotifyActions promise) getAction

future :: Promise a -> Future a
future promise = Future promise

fGet :: Future a -> (IO a -> IO()) -> IO ()
fGet (Future promise) callback = do
   nextAction <- with (pLock promise) $ do
      value_opt <- readIORef (pValue promise)
      case value_opt of
         Just value -> return (callback value)
         Nothing -> do
            pushFront (pNotifyActions promise) callback
            return (return ())

   nextAction

pNotifyValue :: BDeque RealWorld (IO a -> IO()) -> IO a -> IO ()
pNotifyValue deque getValue = do
   notifyAction_opt <- popBack deque
   case notifyAction_opt of
      Just notifyAction -> do
         notifyAction getValue
         pNotifyValue deque getValue
      Nothing -> return ()
