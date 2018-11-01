{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Esferixis.Control.Monad.SafeT(
     SafeT(LiftedSafeT, BindSafeT)
   , runSafeT
   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

data SafeT e m a where
   LiftedSafeT :: m a -> SafeT e m a
   BindSafeT :: SafeT e m b -> ( b -> SafeT e m a ) -> SafeT e m a

instance (Monad m) => Monad (SafeT e m) where
   preSafeT >>= k = BindSafeT preSafeT k

   return value = lift $ return value
   
   fail message = lift $ fail message

instance (Monad m) => Applicative (SafeT e m) where
   pure = return
   (<*>) = ap

instance (Monad m) => Functor (SafeT e m) where
   fmap = liftM

instance MonadTrans (SafeT e) where
   lift unlifted = LiftedSafeT unlifted

runSafeT :: (Monad m) => SafeT e m a -> m a
runSafeT (LiftedSafeT unlifted) = unlifted
runSafeT (BindSafeT preSafeT fun ) = do
   preValue <- runSafeT preSafeT
   runSafeT $ fun preValue
