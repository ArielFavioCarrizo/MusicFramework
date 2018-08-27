{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Esferixis.Control.Concurrency.FutureIO(FutureIO, AsyncIO, runFutureIO, await) where

import Data.Either
import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Esferixis.Control.Concurrency.Promise

{-
   Mónada para los futuros, implementada sobre la mónada IO.
   Sirve para simplificar la programación con promesas, evitando
   así el 'callback hell'.
   Se asemeja a un encadenamiento con instrucciones 'await'
   de lenguajes imperativos como C# y javascript.
-}
data FutureIO a = FutureIO ( IO (Future a) )

-- Alias para representar una mónada FutureIO con un futuro.
type AsyncIO a = FutureIO (Future a)

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
