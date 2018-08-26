{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Esferixis.Concurrency.Promise(Promise, Future, newPromise, newCompletedPromise, newFuture, pSet, future, fGet, fWait) where

import Data.Maybe
import Data.Mutable
import Control.Exception
import Control.Concurrent.MVar

{-
   En estado sin inicializar contiene una cola de funciones
   de notificación.
   Caso contrario contiene una función para obtener el valor
-}
data PromiseState a = UnitializedPromiseState (BDeque RealWorld (IO a -> IO())) | InitializedPromiseState (IO a)

data Promise a = Promise (MVar (PromiseState a))

data Future a = Future (MVar (PromiseState a))

newPromise :: IO (Promise a)
newPromise = do
   notifyActions <- newColl
   stateRef <- newMVar (UnitializedPromiseState notifyActions)
   return ( Promise stateRef )

newCompletedPromise :: a -> IO (Promise a)
newCompletedPromise value = do
   stateRef <- newMVar (InitializedPromiseState (return value))
   return ( Promise stateRef )

newFuture :: (Promise a -> IO ()) -> IO (Future a)
newFuture action = do
   promise <- newPromise
   return ( future promise )

pSet :: forall a. (Promise a -> IO a -> IO ())
pSet (Promise stateMVar) ioaction = do
   result <- (try ioaction) :: IO (Either SomeException a)
   let getAction = 
          case result of
             Left e -> throwIO e
             Right value -> return value

   notifyActions <- modifyMVar stateMVar $ \state ->
      case state of
         UnitializedPromiseState notifyActions -> return (InitializedPromiseState getAction, notifyActions)
         InitializedPromiseState value -> fail "Cannot set value when it has been set"

   pNotifyValue notifyActions getAction

future :: Promise a -> Future a
future (Promise stateMVar) = Future stateMVar

-- Agrega un callback que es invocado cuando el futuro se complete
fGet :: Future a -> (IO a -> IO()) -> IO ()
fGet (Future stateMVar) callback = do
   nextAction <- withMVar stateMVar $ \state ->
      case state of
         UnitializedPromiseState notifyActions -> do
            pushFront notifyActions callback
            return (return ())
         InitializedPromiseState value -> return (callback value)

   nextAction

-- Bloquea el thread hasta que el futuro se complete
fWait :: Future a -> IO a
fWait future = do
   getActionMVar <- newEmptyMVar
   fGet future $ \action -> putMVar getActionMVar action
   getAction <- takeMVar getActionMVar
   getAction

pNotifyValue :: BDeque RealWorld (IO a -> IO()) -> IO a -> IO ()
pNotifyValue deque getValue = do
   notifyAction_opt <- popBack deque
   case notifyAction_opt of
      Just notifyAction -> do
         notifyAction getValue
         pNotifyValue deque getValue
      Nothing -> return ()
