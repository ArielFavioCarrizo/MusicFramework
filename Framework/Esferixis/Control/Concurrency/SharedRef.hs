{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Esferixis.Control.Concurrency.SharedRef(
     SharedRef
   , mkSharedRef
   , srSplit
   , srConsume
   ) where

import Data.Functor
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.Control.Concurrency.RefCount

data SharedRef a = SharedRef {
     sTarget :: a
   , sRefCount :: RefCount
   }

mkSharedRef :: a -> AsyncIO () -> AsyncIO (SharedRef a)
mkSharedRef target deleteTarget = do
   refCount <- mkRefCount deleteTarget

   return
      SharedRef {
           sTarget = target
         , sRefCount = refCount
         }

srSplit :: SharedRef a -> AsyncIO ( SharedRef a, SharedRef a )
srSplit shared = do
   refCountIncRef $ sRefCount shared
   return ( shared, shared )

instance Functor SharedRef where
   fmap fun shared =
      shared { sTarget = fun $ sTarget shared }

srConsume :: SharedRef a -> ( a -> AsyncIO () ) -> AsyncIO ()
srConsume shared fun = do
   fun $ sTarget shared
   refCountDecRef $ sRefCount shared
