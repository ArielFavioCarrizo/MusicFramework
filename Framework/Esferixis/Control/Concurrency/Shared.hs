{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Esferixis.Control.Concurrency.Shared(
     Shared
   , mkShared
   , sSplitRef
   , sConsume
   ) where

import Data.Functor
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.Control.Concurrency.RefCount

data Shared a = Shared {
     sTarget :: a
   , sRefCount :: RefCount
   }

mkShared :: a -> AsyncIO () -> AsyncIO (Shared a)
mkShared target deleteTarget = do
   refCount <- mkRefCount deleteTarget

   return
      Shared {
           sTarget = target
         , sRefCount = refCount
         }

sSplitRef :: Shared a -> AsyncIO ( Shared a, Shared a )
sSplitRef shared = do
   refCountIncRef $ sRefCount shared
   return ( shared, shared )

instance Functor Shared where
   fmap fun shared =
      shared { sTarget = fun $ sTarget shared }

sConsume :: Shared a -> ( a -> AsyncIO () ) -> AsyncIO ()
sConsume shared fun = do
   fun $ sTarget shared
   refCountDecRef $ sRefCount shared
