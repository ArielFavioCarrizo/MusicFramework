{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Bindings.STK.OneZero
   ( OneZero
   , newOneZero
   , deleteOneZero
   , oneZeroSetGain
   , oneZeroSetB0
   , oneZeroSetB1
   , oneZeroSetCoefficients
   , oneZeroSetZero
   , oneZeroTickInplace
   , oneZeroTick
   , onezeroTickSubInplace
   , onezeroTickSub ) where

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

data NativeOneZero
type OneZeroPtr = Ptr NativeOneZero

data OneZero = OneZero (ForeignPtr NativeOneZero)
oneZeroForeignPtr (OneZero a) = a

exceptionSafeSelfAction oneZero nativeFun actionFun = withForeignPtr ( oneZeroForeignPtr oneZero ) (\c_oneZeroPtr -> actionFun ( nativeFun c_oneZeroPtr ) )

foreign import ccall "emfb_stk_onezero_new" c_emfb_stk_onezero_new :: Ptr ExceptDescPtr -> CDouble -> IO OneZeroPtr
foreign import ccall "&emfb_stk_onezero_delete" c_emfb_stk_onezero_delete_ptr :: FunPtr ( OneZeroPtr -> IO () )
foreign import ccall unsafe "emfb_stk_onezero_setGain" c_emfb_stk_onezero_setGain :: OneZeroPtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_onezero_setB0" c_emfb_stk_onezero_setB0 :: OneZeroPtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_onezero_setB1" c_emfb_stk_onezero_setB1 :: OneZeroPtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_onezero_setCoefficients" c_emfb_stk_onezero_setCoefficients :: OneZeroPtr -> CDouble -> CDouble -> CInt -> IO ()
foreign import ccall unsafe "emfb_stk_onezero_setZero" c_emfb_stk_onezero_setZero :: OneZeroPtr -> CDouble -> IO ()
foreign import ccall "emfb_stk_onezero_tickInplace" c_emfb_stk_onezero_tickInplace :: OneZeroPtr -> StkFramesPtr -> CUInt -> IO ()
foreign import ccall "emfb_stk_onezero_tick" c_emfb_stk_onezero_tick :: OneZeroPtr -> StkFramesPtr -> StkFramesPtr -> CUInt -> CUInt -> IO ()
foreign import ccall "emfb_stk_onezero_tickSubInplace" c_emfb_stk_onezero_tickSubInplace :: OneZeroPtr -> StkFramesPtr -> CUInt -> CUInt -> CUInt -> IO ()
foreign import ccall "emfb_stk_onezero_tickSub" c_emfb_stk_onezero_tickSub :: OneZeroPtr -> StkFramesPtr -> StkFramesPtr -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()

newOneZero :: Double -> IO OneZero
newOneZero thePole = withCurriedStkExceptHandlingNewObject_partial (\foreignPtr -> OneZero foreignPtr) c_emfb_stk_onezero_delete_ptr c_emfb_stk_onezero_new $ \fun -> fun (CDouble thePole)

deleteOneZero :: OneZero -> IO ()
deleteOneZero = deleteStkObject oneZeroForeignPtr

createSetValue :: ( OneZeroPtr -> CDouble -> IO () ) -> ( OneZero -> Double -> IO () )
createSetValue = setter exceptionSafeSelfAction

oneZeroSetGain = createSetValue c_emfb_stk_onezero_setGain
oneZeroSetB0 = createSetValue c_emfb_stk_onezero_setB0
oneZeroSetB1 = createSetValue c_emfb_stk_onezero_setB1

oneZeroSetCoefficients :: OneZero -> Double -> Double -> Bool -> IO ()
oneZeroSetCoefficients oneZero b0 b1 clearState = exceptionSafeSelfAction oneZero c_emfb_stk_onezero_setCoefficients $ \fun -> fun ( CDouble b0 ) ( CDouble b1 ) ( hsctypeconvert clearState )

oneZeroSetZero = createSetValue c_emfb_stk_onezero_setZero

oneZeroTickInplace :: OneZero -> StkFrames -> Word32 -> IO ()
oneZeroTickInplace = createStkFramesTickInplaceFun exceptionSafeSelfAction c_emfb_stk_onezero_tickInplace

oneZeroTick :: OneZero -> StkFrames -> StkFrames -> Word32 -> Word32 -> IO ()
oneZeroTick = createStkFramesTickFun exceptionSafeSelfAction c_emfb_stk_onezero_tick

onezeroTickSubInplace :: OneZero -> StkFrames -> Word32 -> Word32 -> Word32 -> IO ()
onezeroTickSubInplace = createStkFramesTickSubInplaceFun exceptionSafeSelfAction c_emfb_stk_onezero_tickSubInplace

onezeroTickSub :: OneZero -> StkFrames -> StkFrames -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
onezeroTickSub = createStkFramesTickSubFun exceptionSafeSelfAction c_emfb_stk_onezero_tickSub
