{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Esferixis.Control.Concurrency.Promise(
     FutureValue
   , Promise
   , Future
   , newPromise
   , newCompletedFuture
   , newFuture
   , pSet
   , pSetFromResult
   , pFuture
   , fGet
   , fWait
   ) where

import Data.Maybe
import Data.Mutable
import Control.Exception
import Control.Concurrent.MVar
import Esferixis.Control.IO

type FutureValue a = Either SomeException a

{-
   En estado sin inicializar contiene una cola de funciones
   de notificación.
   Caso contrario contiene una función para obtener el valor.
-}
data PromiseState a = UnitializedPromiseState (BDeque RealWorld (FutureValue a -> IO())) | InitializedPromiseState (FutureValue a)

data Promise a = Promise (MVar (PromiseState a))

data Future a = VariableFuture (MVar (PromiseState a)) | CompletedFuture (FutureValue a)

newPromise :: IO (Promise a)
newPromise = do
   notifyActions <- newColl
   stateRef <- newMVar (UnitializedPromiseState notifyActions)
   return ( Promise stateRef )

-- Crea un futuro completado con la acción especificada
newCompletedFuture :: IO a -> IO (Future a)
newCompletedFuture action = do
   value <- try action
   return (CompletedFuture value)

-- Crea un futuro con la acción que completa una promesa
newFuture :: (Promise a -> IO ()) -> IO (Future a)
newFuture action = do
   promise <- newPromise
   action promise
   return ( pFuture promise )

-- Completa la promesa con la acción especificada
pSet :: Promise a -> IO a -> IO ()
pSet (Promise stateMVar) ioaction = do
   result <- try ioaction
   pSetFromResult (Promise stateMVar) result

-- Completa la promesa con un resultado de acción
pSetFromResult :: Promise a -> FutureValue a -> IO ()
pSetFromResult (Promise stateMVar) result = do
   notifyActions <- modifyMVar stateMVar $ \state ->
      case state of
         UnitializedPromiseState notifyActions -> return (InitializedPromiseState result, notifyActions)
         InitializedPromiseState value -> fail "Cannot set value when it has been set"

   pNotifyValue notifyActions result

-- Devuelve el futuro de la promesa
pFuture :: Promise a -> Future a
pFuture (Promise stateMVar) = VariableFuture stateMVar

-- Agrega un callback que es invocado cuando el futuro se completa
fGet :: Future a -> (FutureValue a -> IO()) -> IO ()
fGet (VariableFuture stateMVar) callback = do
   nextAction <- withMVar stateMVar $ \state ->
      case state of
         UnitializedPromiseState notifyActions -> do
            pushFront notifyActions callback
            return (return ())
         InitializedPromiseState value -> return (callback value)

   nextAction
fGet (CompletedFuture value) callback = callback value

-- Bloquea el thread hasta que el futuro se complete
fWait :: Future a -> IO a
fWait future = do
   valueMVar <- newEmptyMVar
   fGet future $ \action -> putMVar valueMVar action
   value <- takeMVar valueMVar
   eitherIOReturn value

-- Notifica el valor en los callbacks
pNotifyValue :: BDeque RealWorld (FutureValue a -> IO()) -> FutureValue a -> IO ()
pNotifyValue deque value = do
   notifyAction_opt <- popBack deque
   case notifyAction_opt of
      Just notifyAction -> do
         notifyAction value
         pNotifyValue deque value
      Nothing -> return ()
