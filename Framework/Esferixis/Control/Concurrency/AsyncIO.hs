{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Esferixis.Control.Concurrency.AsyncIO(AsyncIO, await) where

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
data AsyncIO a where
   AwaitAsyncIO :: Future a -> AsyncIO a
   SyncAsyncIO :: IO a -> AsyncIO a
   BindAsyncIO :: AsyncIO b -> ( b -> AsyncIO a ) -> AsyncIO a

{-
   Ejecuta una mónada AsyncIO con el callback
   especificado, en IO
-}
runAsyncIOCPS :: AsyncIO a -> (IO a -> IO ()) -> IO ()
runAsyncIOCPS (AwaitAsyncIO future) postCallback = fGet future postCallback
runAsyncIOCPS (SyncAsyncIO action) postCallback = postCallback action
runAsyncIOCPS (BindAsyncIO preAsyncIO postFun) postCallback =
   case preAsyncIO of
      AwaitAsyncIO preFuture ->
         fGet preFuture $ \preAction -> do
             eitherPreValue <- (try preAction) :: IO (Either SomeException _)
             case eitherPreValue of
                Left e -> postCallback (throwIO e)
                Right preValue -> runAsyncIOCPS (postFun preValue) postCallback

      SyncAsyncIO preAction -> do
         eitherPreValue <- (try preAction) :: IO (Either SomeException _)
         case eitherPreValue of
            Left e -> postCallback (throwIO e)
            Right preValue -> runAsyncIOCPS (postFun preValue) postCallback

      BindAsyncIO preAsyncIO2 postFun2 ->
         runAsyncIOCPS preAsyncIO2 $ \preAction2 -> do
            eitherPreValue <- (try preAction2) :: IO (Either SomeException _)
            case eitherPreValue of
               Left e -> postCallback (throwIO e)
               Right preValue -> runAsyncIOCPS ( postFun2 preValue ) postCallback

{-
   Dado el futuro especificado crea
   la mónada AsyncIO que espera
   que el futuro se complete
-}
await :: Future a -> AsyncIO a
await future = AwaitAsyncIO future

instance Monad AsyncIO where
   (>>=) :: forall a b. AsyncIO a -> (a -> AsyncIO b) -> AsyncIO b
   preAsyncIO >>= k = BindAsyncIO preAsyncIO k

   return value = SyncAsyncIO $ return value

instance Applicative AsyncIO where
   pure = return
   (<*>) = ap

instance Functor AsyncIO where
   fmap = liftM

instance MonadIO AsyncIO where
   liftIO action = SyncAsyncIO action
