{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Esferixis.Concurrency.ThreadLoop(ThreadLoop, newThreadLoop, newBoundedThreadLoop, tlRun, tlClose) where

import Esferixis.Concurrency.Promise
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.IORef
import Data.List
import Data.Maybe

data ThreadLoopState =
   ThreadLoopState {
     tlThreadId :: ThreadId
   , tlStopPromise :: Promise ()
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
      , tlStopPromise = stopPromise
      , tlCloseHasBeenRequested = False
      , tlActionsChannel = actionsChannel
      }
   return ( ThreadLoop stateMVar )

{-
   Encola la acción en el bucle de thread.
   Devuelve el resultado en un futuro.
-}
tlRun :: ThreadLoop -> IO a -> IO (Future a)
tlRun (ThreadLoop tlStateMVar) action = do
   withMVar tlStateMVar $ \state -> do
      resultPromise <- newPromise

      if ( tlCloseHasBeenRequested state )
         then pSet resultPromise ( fail "Close has been requested" )
         else writeChan ( tlActionsChannel state ) ( Just ( pSet resultPromise action ) )

      return (pFuture resultPromise)

{-
   Cierra el bucle de thread (El thread termina),
   después de que se hayan terminado las tareas pendientes.
   Devuelve un futuro que se completa al terminar.
-}
tlClose :: ThreadLoop -> IO (Future ())
tlClose (ThreadLoop tlStateMVar) =
   modifyMVar tlStateMVar $ \state -> do
      if ( tlCloseHasBeenRequested state )
         then do
            future <- newCompletedFuture ( fail "Close has been requested" )
            return ( state, future )
         else do
            writeChan ( tlActionsChannel state ) Nothing -- Para que el bucle termine
            return ( state { tlCloseHasBeenRequested = True }, pFuture ( tlStopPromise state ) )

{-
   Realiza las acciones especificadas.
   Termina cuando encuentra un Nothing en el canal de acciones.
-}
doPendingActions :: Chan (Maybe (IO ())) -> IO ()
doPendingActions actionsChannel = do
   actions <- getChanContents actionsChannel
   sequence_ ( map fromJust (takeWhile isJust actions) )
