{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}

module Esferixis.Control.Monad.SafeT(
     SafeT
   , SafeTResult
   , mkSafeTResult
   , runSafeT
   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

-- Mónada para confinar efectos dentro de un sólo thread de ejecución.
data SafeT e m a where
   LiftedSafeT :: m a -> SafeT e m a
   BindSafeT :: SafeT e m b -> ( b -> SafeT e m a ) -> SafeT e m a

-- Resultado dentro de SafeT
data SafeTResult e a = SafeTResult a

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

{-
   Crea un resultado dentro de la mónada SafeT
-}
mkSafeTResult :: (Monad m) => a -> SafeT e m ( SafeTResult e a )
mkSafeTResult value = return $ SafeTResult value

{-
   Ejecute la acción que devuelve un resultado, garantizando
   que la descripción de efectos envueltos bajo SafeT no puedan
   ser manipulados, y que no puedan
   escapar de la mónada.

   Lo hace de la manera siguiente:
   
   * La única función que puede construir el resultado no produce
   otro efecto que producir el resultado a partir
   de un valor.
   Y el resultado sólo puede ser extraído por ésta función.
   
   * Todo valor cuyo tipo que incluya el 'phantom type' e
   de entorno, no puede ser manipulado con las funciones marcadas con 'e'
   fuera de la mónada.
-}
runSafeT :: (Monad m) => (forall e. SafeT e m ( SafeTResult e a ) ) -> m a
runSafeT safeTAction = do
   SafeTResult value <- runSafeTInternal safeTAction
   return value

runSafeTInternal :: (Monad m) => SafeT e m a -> m a
runSafeTInternal (LiftedSafeT unlifted) = unlifted
runSafeTInternal (BindSafeT preSafeT fun) = do
   preValue <- runSafeTInternal preSafeT
   runSafeTInternal $ fun preValue
