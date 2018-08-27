{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Esferixis.Concurrency.Promise(Promise, Future, newPromise, newCompletedFuture, newFuture, pSet, pFuture, fGet, fWait, FutureIO, runFutureIO, await) where

import Data.Maybe
import Data.Mutable
import Control.Exception
import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

{-
   En estado sin inicializar contiene una cola de funciones
   de notificación.
   Caso contrario contiene una función para obtener el valor.
-}
data PromiseState a = UnitializedPromiseState (BDeque RealWorld (IO a -> IO())) | InitializedPromiseState (IO a)

data Promise a = Promise (MVar (PromiseState a))

data Future a = VariableFuture (MVar (PromiseState a)) | CompletedFuture (IO a)

newPromise :: IO (Promise a)
newPromise = do
   notifyActions <- newColl
   stateRef <- newMVar (UnitializedPromiseState notifyActions)
   return ( Promise stateRef )

-- Crea un futuro completado con la acción especificada
newCompletedFuture :: IO a -> IO (Future a)
newCompletedFuture action = do
   getValue <- wrapResult action
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
   getAction <- wrapResult ioaction

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
fGet (CompletedFuture value) callback = callback value

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

-- Enwrappea la acción, y sólo almacena el resultado
wrapResult :: forall a. (IO a -> IO (IO a))
wrapResult action = do
   result <- (try action) :: IO (Either SomeException a)
   let getAction = 
          case result of
             Left e -> throwIO e
             Right value -> return value
   
   return getAction

{-
   Mónada para los futuros, implementada sobre la mónada IO.
   Sirve para simplificar la programación con promesas, evitando
   así el 'callback hell'.
   Se asemeja a un encadenamiento con instrucciones 'await'
   de lenguajes imperativos como C# y javascript.
-}
data FutureIO a = FutureIO ( IO (Future a) )

{-
   Dada la mónada FutureIO, devuelve
   la mónada IO que la ejecuta
-}
runFutureIO :: FutureIO a -> IO (Future a)
runFutureIO (FutureIO ioAction) = ioAction

{-
   Dado el futuro especificado crea
   la mónada de FutureIO que espera
   que el futuro se complete
-}
await :: Future a -> FutureIO a
await future = FutureIO ( return future )

instance Monad FutureIO where
   (>>=) :: forall a b. FutureIO a -> (a -> FutureIO b) -> FutureIO b
   FutureIO getOldFuture >>= k = FutureIO $ do
      newFuture $ \nextPromise -> do
         oldFuture <- getOldFuture
   
         fGet oldFuture $ \getOldValue -> do
             oldValueResult <- try getOldValue :: IO (Either SomeException a)
             case oldValueResult of
                Left e -> pSet nextPromise ( throwIO e )
                Right oldValue -> do
                   nextFuture <- runFutureIO (k oldValue)
                   pSetFromFuture nextPromise nextFuture

   return value = FutureIO ( newCompletedFuture ( return value ) )

instance Applicative FutureIO where
   pure = return
   (<*>) = ap

instance Functor FutureIO where
   fmap = liftM

instance MonadIO FutureIO where
   liftIO action = FutureIO ( newCompletedFuture action )
