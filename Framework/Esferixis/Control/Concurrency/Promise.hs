{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Esferixis.Control.Concurrency.Promise(
     Promise
   , Future
   , newPromise
   , newFuture
   , pSet
   , pSetFromFuture
   , pFuture
   , fGet
   , fWait
   , fApplyIO
   , fUnwrap
   , fApplyIOFuture
   ) where

import Data.Maybe
import Data.Mutable
import Control.Exception
import Control.Concurrent.MVar
import Esferixis.Control.IO

{-
   En estado sin inicializar contiene una cola de funciones
   de notificación.
   Caso contrario contiene una función para obtener el valor.
-}
data PromiseState a = UnitializedPromiseState (BDeque RealWorld (IO a -> IO())) | InitializedPromiseState (IO a)

data Promise a = Promise (MVar (PromiseState a))

data Future a = Future ( MVar (PromiseState a) )

newPromise :: IO (Promise a)
newPromise = do
   notifyActions <- newColl
   stateRef <- newMVar (UnitializedPromiseState notifyActions)
   return ( Promise stateRef )

-- Crea un futuro con la acción que completa una promesa
newFuture :: (Promise a -> IO ()) -> IO (Future a)
newFuture action = do
   promise <- newPromise
   action promise
   return ( pFuture promise )

-- Completa la promesa con la acción especificada
pSet :: Promise a -> IO a -> IO ()
pSet (Promise stateMVar) ioaction = do
   getAction <- wrapIOResult ioaction

   notifyActions <- modifyMVar stateMVar $ \state ->
      case state of
         UnitializedPromiseState notifyActions -> return (InitializedPromiseState getAction, notifyActions)
         InitializedPromiseState value -> fail "Cannot set value when it has been set"

   pNotifyValue notifyActions getAction

-- Completa la promesa con el valor del futuro al completarse éste
pSetFromFuture :: Promise a -> Future a -> IO ()
pSetFromFuture promise future =
   fGet future ( \value -> pSet promise value ) 

-- Devuelve el futuro de la promesa
pFuture :: Promise a -> Future a
pFuture (Promise stateMVar) = Future stateMVar

-- Agrega un callback que es invocado cuando el futuro se completa
fGet :: Future a -> (IO a -> IO()) -> IO ()
fGet (Future stateMVar) callback = do
   nextAction <- withMVar stateMVar $ \state ->
      case state of
         UnitializedPromiseState notifyActions -> do
            pushFront notifyActions callback
            return (return ())
         InitializedPromiseState value -> return (callback value)

   nextAction

{- Dado un futuro y una función monádica en IO devuelve otro
   otro futuro resultado de la aplicación del
   valor del futuro anterior
-}
fApplyIO :: Future a -> ( a -> IO b ) -> IO (Future b)
fApplyIO ( Future stateMVar ) fun =
   let srcFuture = Future stateMVar
   in newFuture $ \dstPromise -> do
         fGet srcFuture $ \getSrcValue -> do
             pSet dstPromise ( getSrcValue >>= fun )

{- Dado un futuro de futuro lo reduce
   a un futuro de valor literal
-}
fUnwrap :: forall a. (Future (Future a) -> IO (Future a))
fUnwrap ( Future stateMVar ) =
   let srcFutureOfFuture = Future stateMVar
   in newFuture $ \dstPromise -> do
         fGet srcFutureOfFuture $ \getSrcFutureOfFuture -> do
             eitherFuture <- (try getSrcFutureOfFuture) :: IO (Either SomeException (Future a))
             case eitherFuture of
                Left e -> pSet dstPromise (throwIO e)
                Right future -> pSetFromFuture dstPromise future

{- Dado un futuro y una función monádica que devuelve
   un futuro devuelve otro futuro resultado
   de la aplicación del valor del futuro anterior
-}
fApplyIOFuture :: Future a -> ( a -> IO (Future b) ) -> IO (Future b)
fApplyIOFuture srcFuture fun = do
   futureOfFuture <- fApplyIO srcFuture fun
   future <- fUnwrap futureOfFuture

   return future

-- Bloquea el thread hasta que el futuro se complete
fWait :: Future a -> IO a
fWait future = do
   getActionMVar <- newEmptyMVar
   fGet future $ \action -> putMVar getActionMVar action
   getAction <- takeMVar getActionMVar
   getAction

-- Notifica el valor en los callbacks
pNotifyValue :: BDeque RealWorld (IO a -> IO()) -> IO a -> IO ()
pNotifyValue deque getValue = do
   notifyAction_opt <- popBack deque
   case notifyAction_opt of
      Just notifyAction -> do
         notifyAction getValue
         pNotifyValue deque getValue
      Nothing -> return ()
