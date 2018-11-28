{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Esferixis.Control.Concurrency.RefCount(
     RefCount
   , mkRefCount
   , mkRefCountWithCount
   , refCountIncRef
   , refCountDecRef
   ) where

import Data.Word
import Data.Maybe
import Data.IORef
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise

data RefCount = RefCount {
     refCountRefCount :: IORef Word64
   , refCountDelete :: AsyncIO ()
   }

{-
   Crea una cuenta de referencias, que invoca la acción destrución al alcanzar cero.
   La cuenta de referencias inicialmente vale 1.
-}
mkRefCount :: AsyncIO () -> AsyncIO RefCount
mkRefCount deleteAction = mkRefCountWithCount 1 deleteAction

{-
   Crea una cuenta de referencias, que se destruye al alcanzar cero.
   La cuenta tiene que ser positiva
-}
mkRefCountWithCount :: Word64 -> AsyncIO () -> AsyncIO RefCount
mkRefCountWithCount initialCount deleteAction = liftIO $
   if ( initialCount > 0 )
      then do
         refCount <- newIORef initialCount
         return $
            RefCount {
                 refCountRefCount = refCount
               , refCountDelete = deleteAction
               }
      else fail "Reference count must be positive"

{-
   Incrementa la cuenta de referencias
-}
refCountIncRef :: RefCount -> AsyncIO ()
refCountIncRef refCount =
   liftIO $ atomicModifyIORef' (refCountRefCount refCount) $ \oldRefCount -> (oldRefCount+1, ())

{-
   Decrementa la cuenta de referencias
-}
refCountDecRef :: RefCount -> AsyncIO ()
refCountDecRef refCount =
   join $ liftIO $ atomicModifyIORef' (refCountRefCount refCount) $ \oldRefCount ->
      case ( compare oldRefCount 1 ) of
         GT -> (oldRefCount-1, return ())
         EQ -> (0, refCountDelete refCount)
         LT -> (0, fail "Assertion error: Ref count is zero!")
