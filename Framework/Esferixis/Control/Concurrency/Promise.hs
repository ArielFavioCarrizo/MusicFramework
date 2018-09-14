{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Esferixis.Control.Concurrency.Promise(Promise, Future, newPromise, newCompletedFuture, newFuture, pSet, pSetFromFuture, pFuture, fGet, fWait, fApplyIO, fUnwrap, fApplyIOFuture) where

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

data Future a where
   VariableFuture :: MVar (PromiseState a) -> Future a
   CompletedFuture :: IO a -> Future a
   HybridFuture :: Future b -> (IO b -> IO a) -> Future a

newPromise :: IO (Promise a)
newPromise = do
   notifyActions <- newColl
   stateRef <- newMVar (UnitializedPromiseState notifyActions)
   return ( Promise stateRef )

-- Crea un futuro completado con la acción especificada
newCompletedFuture :: IO a -> IO (Future a)
newCompletedFuture action = do
   getValue <- wrapIOResult action
   return (CompletedFuture getValue)

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
pFuture (Promise stateMVar) = VariableFuture stateMVar

-- Agrega un callback que es invocado cuando el futuro se completa
fGet :: Future a -> (IO a -> IO()) -> IO ()
fGet (VariableFuture stateMVar) callback = do
   nextAction <- withMVar stateMVar $ \state ->
      case state of
         UnitializedPromiseState notifyActions -> do
            pushFront notifyActions callback
            return (return ())
         InitializedPromiseState value -> return (callback value)

   nextAction
fGet (HybridFuture previousFuture fun) callback =
   fGet previousFuture $ \oldValue ->
       callback ( fun oldValue )

fGet (CompletedFuture value) callback = callback value

{- Dado un futuro y una función monádica en IO devuelve otro
   otro futuro resultado de la aplicación del
   valor del futuro anterior
-}
fApplyIO :: Future a -> ( a -> IO b ) -> IO (Future b)
fApplyIO ( VariableFuture stateMVar ) fun =
   let srcFuture = VariableFuture stateMVar
   in return $ HybridFuture srcFuture $ \previousIO ->
         previousIO >>= fun

fApplyIO ( HybridFuture previousFuture postFun ) fun =
   return $ HybridFuture previousFuture $ \previousIO ->
      (postFun previousIO) >>= fun

fApplyIO ( CompletedFuture getValue ) fun =
   newCompletedFuture $ do
      value <- getValue
      fun value

{- Dado un futuro de futuro lo reduce
   a un futuro de valor literal
-}
fUnwrap :: forall a. (Future (Future a) -> IO (Future a))
fUnwrap ( VariableFuture stateMVar ) =
   let srcFutureOfFuture = VariableFuture stateMVar
   in newFuture $ \dstPromise -> do
         fGet srcFutureOfFuture $ \getSrcFutureOfFuture -> do
             eitherFuture <- (try getSrcFutureOfFuture) :: IO (Either SomeException (Future a))
             case eitherFuture of
                Left e -> pSet dstPromise (throwIO e)
                Right future -> pSetFromFuture dstPromise future

--fUnwrap ( HybridFuture previousFuture postFun ) =
   

fUnwrap ( CompletedFuture getFutureOfFuture ) = do
   eitherFuture <- (try getFutureOfFuture) :: IO (Either SomeException (Future a))
   case eitherFuture of
      Left e -> newCompletedFuture (throwIO e)
      Right future -> return (future)

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
