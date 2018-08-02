{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Backend.STK
   ( StkValue(cvalue)
   , StkPtr(cptr)
   , StkException(StkException)
   , StkFormat(StkSInt16) ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

import Data.Typeable
import Data.Int

import Control.Exception

foreign import ccall "emfb_stk_sint16" c_emfb_stk_sint16 :: CLong

class StkValue a b where
   cvalue :: a -> b

class StkPtr a b where
   cptr :: a -> Ptr b

data StkException = StkException String
   deriving Typeable
instance Show StkException where
   show (StkException message) = message
instance Exception StkException

data StkFormat = StkSInt16 deriving Show
instance StkValue StkFormat CLong where
   cvalue StkSInt16 = c_emfb_stk_sint16
