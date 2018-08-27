{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Esferixis.Control.IO(wrapIOResult) where

import Control.Exception

-- Enwrappea el resultado de la acción, y sólo almacena el resultado
wrapIOResult :: forall a. (IO a -> IO (IO a))
wrapIOResult action = do
   result <- (try action) :: IO (Either SomeException a)
   let getAction = 
          case result of
             Left e -> throwIO e
             Right value -> return value
   
   return getAction
