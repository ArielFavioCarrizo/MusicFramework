{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Esferixis.Control.Concurrency.AsyncIO(
     AsyncIO
   , runAsyncIO
   , callCC
   , async
   , asyncPSet
   , await
   , forkAsyncIO
   , throwAsyncIO
   , catchAsyncIO
   , tryAsyncIO
   , dropAsyncIO
   ) where

import Data.Either
import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Esferixis.Control.Concurrency.Promise
import Esferixis.Control.IO

{-
   Mónada para programación asíncrona con futuros, implementada sobre la acción IO.
   Sirve para simplificar la programación con promesas y futuros, evitando
   así el 'callback hell'.
   Se asemeja a las funciones asincrónicas con 'async' y 'await'
   de lenguajes imperativos como C# y javascript.

   Representa una acción que potencialmente
   puede consistir en la espera de una acción asincrónica,
   además de las acciones sincrónicas típicas de IO.

   Controla las excepciones producidas tanto por la
   ejecución de las acciones como por evaluaciones
   no exitosas de futuros y de funciones de binding.
-}
data AsyncIO a where
   CallCCAsyncIO :: ( (FutureValue a -> IO ()) -> IO () ) -> AsyncIO a
   AwaitAsyncIO :: Future a -> AsyncIO a
   SyncAsyncIO :: IO a -> AsyncIO a
   CatchAsyncIO :: (Exception e) => AsyncIO a -> (e -> AsyncIO a) -> AsyncIO a
   BindAsyncIO :: AsyncIO b -> ( b -> AsyncIO a ) -> AsyncIO a

instance Monad AsyncIO where
   (>>=) :: forall a b. AsyncIO a -> (a -> AsyncIO b) -> AsyncIO b
   preAsyncIO >>= k = BindAsyncIO preAsyncIO k

   return value = liftIO $ return value
   
   fail message = liftIO $ fail message

instance Applicative AsyncIO where
   pure = return
   (<*>) = ap

instance Functor AsyncIO where
   fmap = liftM

instance MonadIO AsyncIO where
   liftIO action = SyncAsyncIO action

{-
   Ejecuta una acción AsyncIO y devuelve
   su resultado en un futuro, en la acción IO
-}
runAsyncIO :: AsyncIO a -> IO ( Future a )
runAsyncIO asyncIO = newFuture $ \postPromise ->
   tryToRunAsyncIOCPS asyncIO $ \preEitherValue -> pSet postPromise ( eitherIOReturn preEitherValue )

{-
   Dada una acción AsyncIO, crea
   la acción AsyncIO que ejecuta
   asincrónicamente dicha acción.
   El resultado de la acción la
   representa como un futuro.
   
   Cumple el mismo rol que la
   palabra clave 'async' de
   lenguajes imperativos como C#
   y javascript.
-}
async :: AsyncIO a -> AsyncIO (Future a)
async action = liftIO $ runAsyncIO action

{-
   Setea el valor de la promesa ejecutando asincrónicamente la acción AsyncIO especificada.

   Si la promesa está en proceso de ser completada o ya fue completada lanza excepción.
-}
asyncPSet :: Promise a -> AsyncIO a -> AsyncIO ()
asyncPSet promise action = liftIO $ pSetFromResultWithCallback promise $ tryToRunAsyncIOCPS action

{-
   Dado el futuro especificado crea
   la acción AsyncIO que espera
   que el futuro se complete
-}
await :: Future a -> AsyncIO a
await future = AwaitAsyncIO future

{-
   Crea una acción que ejecuta asincrónicamente la
   acción especificada, sin feedback.

   ATENCIÓN: La acción a ejecutar no puede lanzar
   excepciones. Caso contrario lanzará una excepción
   en la mónada IO, con el riesgo potencial
   de que el programa sea abortado.
-}
forkAsyncIO :: AsyncIO () -> AsyncIO ()
forkAsyncIO action = liftIO $ runAsyncIOCPS action $ \eitherValue ->
   case eitherValue of
      Right a -> return ()
      Left e -> throwIO e

{-
   Realiza un acción en IO, cuyo resultado
   se envía en la continuación actual, que la recibe

   ATENCIÓN: Del manejo de excepciones se encarga la acción IO
   recibida.

   Toda excepción producida tiene que pasarse a través de la continuación.
   Caso contrario se obtendrá comportamiento indefinido.
-}
callCC :: ( (FutureValue a -> IO ()) -> IO () ) -> AsyncIO a
callCC ioActionByCC = CallCCAsyncIO ioActionByCC

{-
   Crea una acción AsyncIO que lanza la excepción
   especificada
-}
throwAsyncIO :: (Exception e) => e -> AsyncIO a
throwAsyncIO exception = liftIO $ throwIO exception

{-
   Crea una acción AsyncIO que ejecuta la acción AsyncIO
   especificada, si ocurre una excepción realiza la acción
   AsyncIO que especifica la función de manejo de excepción con
   la excepción ocurrida.
-}
catchAsyncIO :: (Exception e) => AsyncIO a -> ( e -> AsyncIO a ) -> AsyncIO a
catchAsyncIO targetAsyncIO handlerFun = CatchAsyncIO targetAsyncIO handlerFun

{-
   Ejecuta la acción, y da el resultado o la excepción
-}
tryAsyncIO :: (Exception e) => AsyncIO a -> AsyncIO ( Either e a )
tryAsyncIO action = catchAsyncIO ( action >>= \value -> return $ Right value) (\e -> return $ Left e)

{-
   Descarta las excepciones de la acción especificada
-}
dropAsyncIO :: AsyncIO () -> AsyncIO ()
dropAsyncIO action = catchAsyncIO action ( ( const $ return () ) :: SomeException -> AsyncIO () )

{-
   A continuación viene el núcleo de la implementación de la ejecución de AsyncIO
   sobre IO, con Continuation Passing Style (CPS)
-}

{-
   Intenta obtener la acción AsyncIO en 'Weak head normal form',
   y si lo puede a continuación la ejecuta en la acción IO devuelta.
   Si no puede obtener la acción responde con la excepción en la
   continuación (postCallback).
-}
tryToRunAsyncIOCPS :: AsyncIO a -> (FutureValue a -> IO ()) -> IO ()
tryToRunAsyncIOCPS unevaluatedAsyncIO postCallback = do
   evaluatedAsyncIO <- try $ evaluate unevaluatedAsyncIO
   case evaluatedAsyncIO of
      Right asyncIO -> runAsyncIOCPS asyncIO postCallback
      Left e -> postCallback $ Left e

{-
   Ejecuta una acción AsyncIO con el callback
   de resultado especificado, en la acción IO
-}
runAsyncIOCPS :: AsyncIO a -> (FutureValue a -> IO ()) -> IO ()
runAsyncIOCPS (CallCCAsyncIO ioActionByCC) postCallback = ioActionByCC postCallback
runAsyncIOCPS (AwaitAsyncIO unevaluatedFuture) postCallback = do
   evaluatedFuture <- try $ evaluate unevaluatedFuture
   case evaluatedFuture of
      Right future -> fGet future postCallback
      Left e -> postCallback $ Left e
runAsyncIOCPS (SyncAsyncIO unevaluatedAction) postCallback = do
   evaluatedAction <- try $ evaluate unevaluatedAction
   case evaluatedAction of
      Right action -> do    
         value <- try action
         postCallback value
      Left e -> postCallback $ Left e
runAsyncIOCPS (CatchAsyncIO unevaluatedTargetAsyncIO handlerFun) postCallback =
   tryToRunAsyncIOCPS unevaluatedTargetAsyncIO $ \preEitherValue -> do
      case preEitherValue of
         Right preValue -> postCallback $ Right preValue
         Left someException ->
            case ( fromException someException ) of
               Just exceptionToHandle -> tryToRunAsyncIOCPS ( handlerFun exceptionToHandle ) postCallback
               Nothing -> postCallback $ Left someException
runAsyncIOCPS (BindAsyncIO unevaluatedPreAsyncIO postFun) postCallback =
   tryToRunAsyncIOCPS unevaluatedPreAsyncIO $ \preEitherValue -> do
      case preEitherValue of
         Right preValue -> tryToRunAsyncIOCPS ( postFun preValue ) postCallback
         Left e -> postCallback $ Left e
