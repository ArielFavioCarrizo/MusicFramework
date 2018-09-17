{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Esferixis.Control.IO(eitherIOReturn) where

import Control.Exception

{-
   Convierte un valor con posible excepción en una acción
   de obtención de valor
-}
eitherIOReturn :: (Exception e) => Either e a -> IO a
eitherIOReturn either =
   case either of
      Left e -> throwIO e
      Right value -> return value
