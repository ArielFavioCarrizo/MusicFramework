{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Esferixis.Control.Monad.SafeT(
     SafeT
   , runSafeT
   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

-- Mónada para confinar efectos dentro de un sólo thread de ejecución.
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

data SafeTToken = SafeTToken

{-
   Ejecute la acción que devuelve un resultado, garantizando
   que la descripción de efectos envueltos bajo SafeT no puedan
   ser manipulados, y que no puedan
   escapar de la mónada.
-}
runSafeT :: (Monad m) => ( forall e. SafeT e m a ) -> m a
runSafeT safeTAction =
   runSafeTInternal safeTAction

runSafeTInternal :: (Monad m) => SafeT e m a -> m a
runSafeTInternal (LiftedSafeT unlifted) = unlifted
runSafeTInternal (BindSafeT preSafeT fun) = do
   preValue <- runSafeTInternal preSafeT
   runSafeTInternal $ fun preValue
