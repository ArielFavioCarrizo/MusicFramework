{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Esferixis.Control.Concurrency.AsyncIO(AsyncIO, runAsyncIO, await) where

import Data.Either
import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Esferixis.Control.Concurrency.Promise

{-
   Mónada para programación asíncrona con futuros, implementada sobre la mónada IO.
   Sirve para simplificar la programación con promesas, evitando
   así el 'callback hell'.
   Se asemeja a las funciones asincrónicas con 'async' y 'await'
   de lenguajes imperativos como C# y javascript.
-}
data AsyncIO a = AsyncIO ( IO (Future a) )

{-
   Dada la mónada AsyncIO, devuelve
   la mónada IO que la ejecuta.
   Ésta operación devuelve el futuro representado
   por la ejecución de la mónada de entrada.
-}
runAsyncIO :: AsyncIO a -> IO (Future a)
runAsyncIO (AsyncIO ioAction) = ioAction

{-
   Dado el futuro especificado crea
   la mónada AsyncIO que espera
   que el futuro se complete
-}
await :: Future a -> AsyncIO a
await future = AsyncIO ( return future )

instance Monad AsyncIO where
   (>>=) :: forall a b. AsyncIO a -> (a -> AsyncIO b) -> AsyncIO b
   AsyncIO getSrcFuture >>= k = AsyncIO $ do
       srcFuture <- getSrcFuture

       fApplyIOFuture srcFuture $ \srcValue -> runAsyncIO (k srcValue)

   return value = AsyncIO ( newCompletedFuture ( return value ) )

instance Applicative AsyncIO where
   pure = return
   (<*>) = ap

instance Functor AsyncIO where
   fmap = liftM

instance MonadIO AsyncIO where
   liftIO action = AsyncIO ( newCompletedFuture action )
