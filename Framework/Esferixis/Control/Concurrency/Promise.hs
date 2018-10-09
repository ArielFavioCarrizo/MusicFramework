{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Esferixis.Control.Concurrency.Promise(
     FutureValue
   , Promise
   , Future
   , newPromise
   , newCompletedFuture
   , newFuture
   , pSet
   , pSetFromResult
   , pSetFromResultWithCallback
   , pFuture
   , fGet
   , fWait
   ) where

import Data.Maybe
import Data.Mutable
import Control.Exception
import Control.Concurrent.MVar
import Control.Monad
import Esferixis.Control.IO
import Data.Functor

type FutureValue a = Either SomeException a

{-
   En estado sin inicializar contiene una cola de funciones
   de notificación.
   Caso contrario contiene una función para obtener el valor.
-}
data PromiseState a =
   -- Estado sin completar
   UncompletedPromiseState {
        pendingPromiseNotifications :: BDeque RealWorld (FutureValue a -> IO()) -- Funciones de notificaciones pendientes
      , promiseIsOnCompletionProcess :: Bool
      } |
   CompletedPromiseState (FutureValue a) -- Estado de promesa completada

data Promise a = Promise (MVar (PromiseState a))

data Future a where
   StatefulFuture :: Promise a -> Future a
   CompletedFuture :: FutureValue a -> Future a
   MappedFuture :: Future b -> ( b -> a ) -> Future a

newPromise :: IO (Promise a)
newPromise = do
   notifyActions <- newColl
   let firstState = UncompletedPromiseState {
        pendingPromiseNotifications = notifyActions
      , promiseIsOnCompletionProcess = False
      }
   stateRef <- newMVar firstState
   return $ Promise stateRef

-- Crea un futuro completado con la acción especificada
newCompletedFuture :: IO a -> IO (Future a)
newCompletedFuture action = do
   value <- try action
   return $ CompletedFuture value

-- Crea un futuro con la acción que completa una promesa
newFuture :: (Promise a -> IO ()) -> IO (Future a)
newFuture action = do
   promise <- newPromise
   action promise
   return $ pFuture promise

-- Completa la promesa con la acción especificada
pSet :: Promise a -> IO a -> IO ()
pSet (Promise stateMVar) ioaction = do
   result <- try ioaction
   pSetFromResult (Promise stateMVar) result

{-
   Completa la promesa con un resultado de acción.

   Si la promesa está en proceso de ser completada o ya fue completada lanza excepción.
-}
pSetFromResult :: Promise a -> FutureValue a -> IO ()
pSetFromResult (Promise stateMVar) result = do
   join $ modifyMVar stateMVar $ \state ->
      case state of
         UncompletedPromiseState {
              pendingPromiseNotifications = notifyActions
            , promiseIsOnCompletionProcess = isOnCompletionProcess
            } ->
               if ( isOnCompletionProcess )
                  then fail "Promise is on completion process"
                  else return (CompletedPromiseState result, pNotifyValue notifyActions result)

         CompletedPromiseState value -> fail "Promise has been completed"

{- 
   Completa la promesa con un callback que da un resultado de acción
   eventual

   Si la promesa está en proceso de ser completada o ya fue completada lanza excepción.
-}
pSetFromResultWithCallback :: Promise a -> ( ( FutureValue a -> IO () ) -> IO () ) -> IO ()
pSetFromResultWithCallback (Promise stateMVar) callback = do
   join $ modifyMVar stateMVar $ \state ->
      case state of
         UncompletedPromiseState { promiseIsOnCompletionProcess = isOnCompletionProcess } ->
            if ( isOnCompletionProcess )
               then fail "Promise is already on completion process"
               else
                    let nextAction =
                           callback $ \result ->
                              join $ modifyMVar stateMVar $ \state ->
                                 case state of
                                    UncompletedPromiseState {
                                         pendingPromiseNotifications = notifyActions
                                       , promiseIsOnCompletionProcess = isOnCompletionProcess
                                       } ->
                                          if ( isOnCompletionProcess )
                                             then return (CompletedPromiseState result, pNotifyValue notifyActions result)
                                             else fail "Assertion error on pSetFromResultWithCallback: Expected that promise is on completion process"
                                    CompletedPromiseState value -> fail "Assertion error on pSetFromResultWithCallback: Unexpected completed state"

                    in return ( state { promiseIsOnCompletionProcess = True }, nextAction )
         CompletedPromiseState value -> fail "Promise has been completed"

-- Devuelve el futuro de la promesa
pFuture :: Promise a -> Future a
pFuture = StatefulFuture

-- Agrega un callback que es invocado cuando el futuro se completa
fGet :: Future a -> (FutureValue a -> IO()) -> IO ()
fGet (StatefulFuture (Promise stateMVar)) callback = do
   nextAction <- withMVar stateMVar $ \state ->
      case state of
         UncompletedPromiseState { pendingPromiseNotifications = notifyActions } -> do
            pushFront notifyActions callback
            return (return ())
         CompletedPromiseState value -> return (callback value)

   nextAction
fGet (CompletedFuture value) callback = callback value
fGet (MappedFuture srcFuture fun) callback =
   fGet srcFuture $ \srcValue -> do
      dstValue <- case srcValue of
         Right value -> try $ evaluate $ fun value
         Left e -> return $ Left e

      callback dstValue

-- Un valor futuro es un functor
instance Functor Future where
   fmap fun srcFuture = MappedFuture srcFuture fun

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
