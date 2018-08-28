{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Bindings.STK.Delay
   ( Delay
   , newDelay
   , deleteDelay
   , delaySetGain
   , delaySetMaximumDelay
   , delaySetDelay
   , delayTickInplace
   , delayTick ) where

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

data NativeDelay
type DelayPtr = Ptr NativeDelay

data Delay = Delay (ForeignPtr NativeDelay)
delayForeignPtr (Delay a) = a

exceptionUnsafeSelfAction = exceptionUnsafeStkObjectAction delayForeignPtr
exceptionSafeSelfAction = exceptionSafeStkObjectAction delayForeignPtr

exceptionSafeSelfSetter nativeFun = setter exceptionSafeSelfAction nativeFun
exceptionUnsafeSelfSetter nativeFun = exceptionUnsafeSetter exceptionUnsafeSelfAction nativeFun

selfGetter nativeFun = getter exceptionSafeSelfAction nativeFun

foreign import ccall "emfb_stk_delay_new" c_emfb_stk_delay_new :: Ptr ExceptDescPtr -> CDouble -> CULong -> IO DelayPtr
foreign import ccall "&emfb_stk_delay_delete" c_emfb_stk_delay_delete_ptr :: FunPtr ( DelayPtr -> IO () )
foreign import ccall unsafe "emfb_stk_delay_setGain" c_emfb_stk_delay_setGain :: DelayPtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_delay_setMaximumDelay" c_emfb_stk_delay_setMaximumDelay :: DelayPtr -> CULong -> IO ()
foreign import ccall unsafe "emfb_stk_delay_setDelay" c_emfb_stk_delay_setDelay :: Ptr ExceptDescPtr -> DelayPtr -> CULong -> IO ()
foreign import ccall "emfb_stk_delay_tickInplace" c_emfb_stk_delay_tickInplace :: DelayPtr -> StkFramesPtr -> CUInt -> IO ()
foreign import ccall "emfb_stk_delay_tick" c_emfb_stk_delay_tick :: DelayPtr -> StkFramesPtr -> StkFramesPtr -> CUInt -> CUInt -> IO ()
foreign import ccall "emfb_stk_delay_tickSubInplace" c_emfb_stk_delay_tickSubInplace :: DelayPtr -> StkFramesPtr -> CUInt -> CUInt -> CUInt -> IO ()
foreign import ccall "emfb_stk_delay_tickSub" c_emfb_stk_delay_tickSub :: DelayPtr -> StkFramesPtr -> StkFramesPtr -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()

newDelay :: Double -> Word32 -> IO Delay
newDelay delay maxDelay = withCurriedStkExceptHandlingNewObject_partial (\foreignPtr -> Delay foreignPtr) c_emfb_stk_delay_delete_ptr c_emfb_stk_delay_new $ \fun -> fun (CDouble delay) (CULong maxDelay)

deleteDelay :: Delay -> IO ()
deleteDelay = deleteStkObject delayForeignPtr

delaySetGain :: Delay -> Double -> IO ()
delaySetGain = exceptionSafeSelfSetter c_emfb_stk_delay_setGain

delaySetMaximumDelay :: Delay -> Word32 -> IO ()
delaySetMaximumDelay = exceptionSafeSelfSetter c_emfb_stk_delay_setMaximumDelay

delaySetDelay :: Delay -> Word32 -> IO ()
delaySetDelay = exceptionUnsafeSelfSetter c_emfb_stk_delay_setDelay

delayTickInplace :: Delay -> StkFrames -> Word32 -> IO ()
delayTickInplace = createStkFramesTickInplaceFun exceptionSafeSelfAction c_emfb_stk_delay_tickInplace

delayTick :: Delay -> StkFrames -> StkFrames -> Word32 -> Word32 -> IO ()
delayTick = createStkFramesTickFun exceptionSafeSelfAction c_emfb_stk_delay_tick

delayTickSubInplace :: Delay -> StkFrames -> Word32 -> Word32 -> Word32 -> IO ()
delayTickSubInplace = createStkFramesTickSubInplaceFun exceptionSafeSelfAction c_emfb_stk_delay_tickSubInplace

delayTickSub :: Delay -> StkFrames -> StkFrames -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
delayTickSub = createStkFramesTickSubFun exceptionSafeSelfAction c_emfb_stk_delay_tickSub
