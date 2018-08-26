{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Esferixis.Concurrency.ThreadLoop(ThreadLoop) where

import Esferixis.Concurrency.Promise
import Control.Concurrent
import Control.Concurrent.Chan

data ThreadLoopState =
   WorkingThreadLoopState {
     tlThreadId :: ThreadId
   , tlActions :: Chan (IO ())
   } |
   ClosedThreadLoopState

data ThreadLoop = ThreadLoop (MVar ThreadLoopState)

newThreadLoop :: ( IO () -> IO ThreadId ) -> IO ThreadLoop
newThreadLoop newThreadFork = do
   actions <- newChan
   threadId <- newThreadFork ( doPendingActions actions )
   stateMVar <- newMVar WorkingThreadLoopState {
        tlThreadId = threadId
      , tlActions = actions
      }
   return ( ThreadLoop stateMVar )

doPendingActions :: Chan (IO ()) -> IO ()
doPendingActions actions = do
   return () -- FIXME: Completar
