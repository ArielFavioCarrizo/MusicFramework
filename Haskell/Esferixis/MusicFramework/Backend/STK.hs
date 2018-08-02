{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Backend.STK
   ( StkValue(cvalue)
   , StkException(StkException)
   , StkFormat(StkSInt16) ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

import Data.Typeable
import Data.Int

import Foreign.ForeignPtr
import Control.Exception

class StkValue a b where
   cvalue :: a -> b

foreign import ccall "emfb_stk_sint16" c_emfb_stk_sint16 :: CULong

data StkException = StkException String
   deriving Typeable
instance Show StkException where
   show (StkException message) = message
instance Exception StkException

data StkFormat = StkSInt16 deriving Show
instance StkValue StkFormat CULong where
   cvalue StkSInt16 = c_emfb_stk_sint16
