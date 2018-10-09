{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Esferixis.Control.Concurrency.ThreadLoop(ThreadLoop, newThreadLoop, newBoundedThreadLoop, tlRun, tlClose) where

import Esferixis.Control.Concurrency.Promise
import Esferixis.Control.Concurrency.AsyncIO
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.IORef
import Data.List
import Data.Maybe

data ThreadLoopState =
   ThreadLoopState {
     tlThreadId :: ThreadId
   , tlCloseHasBeenRequested :: Bool
   , tlActionsChannel :: Chan (Maybe (IO ()))
   }

data ThreadLoop = ThreadLoop (MVar ThreadLoopState)

-- Crea un bucle de thread con 'forkIO'
newThreadLoop = newThreadLoopGen forkIO

-- Crea un bucle de thread con 'forkOS'
newBoundedThreadLoop = newThreadLoopGen forkOS

{-
   Crea un bucle de thread (Un thread con un bucle de realización de acciones),
   con la instrucción de forkeo de thread especificada
-}
newThreadLoopGen :: ( IO () -> IO ThreadId ) -> IO ThreadLoop
newThreadLoopGen chosenFork = do
   actionsChannel <- newChan
   stopPromise <- newPromise
   threadId <- chosenFork ( doPendingActions actionsChannel >> pSet stopPromise ( return () ) )
   stateMVar <- newMVar ThreadLoopState {
        tlThreadId = threadId
      , tlCloseHasBeenRequested = False
      , tlActionsChannel = actionsChannel
      }
   return $ ThreadLoop stateMVar

{-
   Crea una acción AsyncIO que ejecuta
   la continuación en el ThreadLoop especificado
-}
tlRun :: ThreadLoop -> AsyncIO ()
tlRun (ThreadLoop tlStateMVar) = callCC $ \continuation ->
   join $ withMVar tlStateMVar $ \state ->
      if ( tlCloseHasBeenRequested state )
         then
            return $ (try $ fail "Close has been requested") >>= continuation
         else do
            writeChan ( tlActionsChannel state ) $ Just $ continuation $ Right () -- Le pasa el control a la continuación en el loop del thread
            return $ return ()

{-
   Crea una acción AsyncIO que continúa
   la continuación en el ThreadLoop especificado,
   Una vez que no haya tareas anteriores pendientes.
   Asegurando que una vez que se produzca el
   cambio de thread, el bucle de thread se cierre.
-}
tlClose :: ThreadLoop -> AsyncIO ()
tlClose (ThreadLoop tlStateMVar) = callCC $ \continuation ->
   join $ modifyMVar tlStateMVar $ \state ->
      if ( tlCloseHasBeenRequested state )
         then
            let postAction = (try $ fail "Close has been requested") >>= continuation
            in return ( state, postAction )
         else do
            let sendAction = writeChan $ tlActionsChannel state

            sendAction $ Just $ continuation $ Right () -- Le pasa el control a la continuación en el loop del thread
            sendAction Nothing -- Asegura que después de que se produzca un cambio de thread, el bucle de thread se cierre

            return ( state { tlCloseHasBeenRequested = True }, return () )

{-
   Realiza las acciones especificadas.
   Termina cuando encuentra un Nothing en el canal de acciones.
-}
doPendingActions :: Chan (Maybe (IO ())) -> IO ()
doPendingActions actionsChannel = do
   actions <- getChanContents actionsChannel
   sequence_ $ map fromJust $ takeWhile isJust actions
