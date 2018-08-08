{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Bindings.STK
   ( StkValue(cvalue)
   , StkException(StkException)
   , StkFormat(StkSInt16)
   , sampleRate
   , setSampleRate ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types

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

foreign import ccall "emfb_stk_sampleRate" c_emfb_stk_sampleRate :: IO CDouble
foreign import ccall "emfb_stk_setSampleRate" c_emfb_stk_setSampleRate :: CDouble -> IO ()

sampleRate :: IO Double
sampleRate = do
   c_sampleRate <- c_emfb_stk_sampleRate
   return ( uncurry encodeFloat ( decodeFloat c_sampleRate ) )

setSampleRate :: Double -> IO ()
setSampleRate rate = c_emfb_stk_setSampleRate ( CDouble rate )
