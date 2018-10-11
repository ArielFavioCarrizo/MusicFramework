{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Esferixis.Control.Concurrency.RefCounted(
     RefCounted
   , mkRefCounted
   , mkRefCountedWithCount
   , refCountedIncRef
   , refCountedUse
   , refCountedConsume
   ) where

import Data.Word
import Data.Maybe
import Data.IORef
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.MusicFramework.Signal.IO.FramesChunk

data RefCounted a = RefCounted {
     refCountedObj :: a
   , refCountedRefCount :: IORef Word64
   , refCountedDelete :: AsyncIO ()
   }

{-
   Crea una envoltura del objeto con cuenta de referencias, que se destruye al alcanzar cero.
   La cuenta tiene que ser positiva.
   La cuenta de referencias inicialmente vale 1.
-}
mkRefCounted :: a -> AsyncIO () -> AsyncIO ( RefCounted a )
mkRefCounted object deleteFun = mkRefCountedWithCount object 1 deleteFun

{-
   Crea una envoltura del objeto con cuenta de referencias, que se destruye al alcanzar cero.
   La cuenta tiene que ser positiva
-}
mkRefCountedWithCount :: a -> Word64 -> AsyncIO () -> AsyncIO ( RefCounted a )
mkRefCountedWithCount object initialCount deleteFun = liftIO $
   if ( initialCount > 0 )
      then do
         refCount <- newIORef initialCount
         return $
            RefCounted {
                 refCountedObj = object
               , refCountedRefCount = refCount
               , refCountedDelete = deleteFun
               }
      else fail "Reference count must be positive"

{-
   Incrementa la cuenta de referencias
-}
refCountedIncRef :: RefCounted sc -> AsyncIO ()
refCountedIncRef refCounted =
   liftIO $ atomicModifyIORef' (refCountedRefCount refCounted) $ \oldRefCount -> (oldRefCount+1, ())

{-
   Usa el objeto sin cambiar la cuenta de referencias
-}
refCountedUse :: RefCounted sc -> ( sc -> AsyncIO r ) -> AsyncIO r
refCountedUse refCounted fun =
   fun $ refCountedObj refCounted

{-
   Consume el objeto. Decrementa la cuenta de referencias.
   Cuando alcanza cero el objeto es destruido automáticamente.
-}
refCountedConsume :: RefCounted sc -> ( sc -> AsyncIO r ) -> AsyncIO r
refCountedConsume refCounted fun =
   finallyAsyncIO (refCountedUse refCounted fun) $
      join $ liftIO $ atomicModifyIORef' (refCountedRefCount refCounted) $ \oldRefCount ->
         case ( compare oldRefCount 1 ) of
            GT -> (oldRefCount-1, return ())
            EQ -> (0, refCountedDelete refCounted)
            LT -> (0, fail "Assertion error: Ref count is zero!")
