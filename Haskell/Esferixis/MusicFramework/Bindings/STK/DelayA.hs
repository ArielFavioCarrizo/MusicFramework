{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Bindings.STK.DelayA
   ( DelayA
   , newDelayA
   , deleteDelayA
   , delayASetGain
   , delayASetB0
   , delayASetA1
   , delayASetCoefficients
   , delayASetPole
   , delayATickInplace
   , delayATick ) where

import Data.Word
import Data.Int

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.C.String
import Foreign.ForeignPtr

import Control.Exception

import Esferixis.MusicFramework.Bindings.STK.Internal.Misc
import Esferixis.MusicFramework.Bindings.STK.Frames

import Esferixis.Foreign.CTypes
import Esferixis.Foreign.Objects

data NativeDelayA
type DelayAPtr = Ptr NativeDelayA

data DelayA = DelayA (ForeignPtr NativeDelayA)
delayAForeignPtr (DelayA a) = a

unhandledDelayAAction delayA nativeFun actionFun = withForeignPtr ( delayAForeignPtr delayA ) (\c_delayAPtr -> actionFun ( nativeFun c_delayAPtr ) )

foreign import ccall "emfb_stk_delaya_new" c_emfb_stk_delaya_new :: Ptr ExceptDescPtr -> CDouble -> CULong -> IO DelayAPtr
foreign import ccall "&emfb_stk_delaya_delete" c_emfb_stk_delaya_delete_ptr :: FunPtr ( DelayAPtr -> IO () )
foreign import ccall unsafe "emfb_stk_delaya_setGain" c_emfb_stk_delaya_setGain :: DelayAPtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_delaya_clear" c_emfb_stk_delaya_clear :: DelayAPtr -> IO ()
foreign import ccall unsafe "emfb_stk_delaya_getMaximumDelay" c_emfb_stk_delaya_getMaximumDelay :: DelayAPtr -> IO CULong
foreign import ccall unsafe "emfb_stk_delaya_setMaximumDelay" c_emfb_stk_delaya_setMaximumDelay :: DelayAPtr -> CULong -> IO ()
foreign import ccall unsafe "emfb_stk_delaya_setDelay" c_emfb_stk_delaya_setDelay :: Ptr ExceptDescPtr -> DelayAPtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_delaya_getDelay" c_emfb_stk_delaya_getDelay :: DelayAPtr -> IO CDouble
foreign import ccall "emfb_stk_delaya_tickInplace" c_emfb_stk_delaya_tickInplace :: DelayAPtr -> StkFramesPtr -> CUInt -> IO ()
foreign import ccall "emfb_stk_delaya_tick" c_emfb_stk_delaya_tick :: DelayAPtr -> StkFramesPtr -> StkFramesPtr -> CUInt -> CUInt -> IO ()

newDelayA :: CDouble -> Word64 -> IO DelayA
newDelayA delay maxDelay = withCurriedStkExceptHandlingNewObject_partial (\foreignPtr -> DelayA foreignPtr) c_emfb_stk_delaya_delete_ptr c_emfb_stk_delaya_new (\fun -> fun (CDouble delay) (CULong maxDelay) )

deleteDelayA :: DelayA -> IO ()
deleteDelayA = deleteStkObject delayAForeignPtr

-- FIXME: Hacer bindings de los demás métodos

delayATickInplace :: DelayA -> StkFrames -> Word32 -> IO ()
delayATickInplace = createStkFramesTickInplaceFun unhandledDelayAAction c_emfb_stk_delaya_tickInplace

delayATick :: DelayA -> StkFrames -> StkFrames -> Word32 -> Word32 -> IO ()
delayATick = createStkFramesTickFun unhandledDelayAAction c_emfb_stk_delaya_tick
