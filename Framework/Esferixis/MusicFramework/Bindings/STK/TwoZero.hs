{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Bindings.STK.TwoZero
   ( TwoZero
   , newTwoZero
   , deleteTwoZero
   , twoZeroSetGain
   , twoZeroIgnoreSampleRateChange
   , twoZeroSetB0
   , twoZeroSetB1
   , twoZeroSetB2
   , twoZeroSetCoefficients
   , twoZeroSetNotch
   , twoZeroTickInplace
   , twoZeroTick
   , twozeroTickSubInplace
   , twozeroTickSub ) where

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

data NativeTwoZero
type TwoZeroPtr = Ptr NativeTwoZero

data TwoZero = TwoZero (ForeignPtr NativeTwoZero)
twoZeroForeignPtr (TwoZero a) = a

exceptionSafeSelfAction twoZero nativeFun actionFun = withForeignPtr ( twoZeroForeignPtr twoZero ) (\c_twoZeroPtr -> actionFun ( nativeFun c_twoZeroPtr ) )

foreign import ccall "emfb_stk_twozero_new" c_emfb_stk_twozero_new :: Ptr ExceptDescPtr -> CDouble -> IO TwoZeroPtr
foreign import ccall "&emfb_stk_twozero_delete" c_emfb_stk_twozero_delete_ptr :: FunPtr ( TwoZeroPtr -> IO () )
foreign import ccall unsafe "emfb_stk_twozero_setGain" c_emfb_stk_twozero_setGain :: TwoZeroPtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_twozero_ignoreSampleRateChange" c_emfb_stk_twozero_ignoreSampleRateChange :: TwoZeroPtr -> CInt -> IO ()
foreign import ccall unsafe "emfb_stk_twozero_setB0" c_emfb_stk_twozero_setB0 :: TwoZeroPtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_twozero_setB1" c_emfb_stk_twozero_setB1 :: TwoZeroPtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_twozero_setB2" c_emfb_stk_twozero_setB2 :: TwoZeroPtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_twozero_setCoefficients" c_emfb_stk_twozero_setCoefficients :: TwoZeroPtr -> CDouble -> CDouble -> CDouble -> CInt -> IO ()
foreign import ccall unsafe "emfb_stk_twozero_setResonance" c_emfb_stk_twozero_setNotch :: TwoZeroPtr -> CDouble -> CDouble -> IO ()
foreign import ccall "emfb_stk_twozero_tickInplace" c_emfb_stk_twozero_tickInplace :: TwoZeroPtr -> StkFramesPtr -> CUInt -> IO ()
foreign import ccall "emfb_stk_twozero_tick" c_emfb_stk_twozero_tick :: TwoZeroPtr -> StkFramesPtr -> StkFramesPtr -> CUInt -> CUInt -> IO ()
foreign import ccall "emfb_stk_twozero_tickSubInplace" c_emfb_stk_twozero_tickSubInplace :: TwoZeroPtr -> StkFramesPtr -> CUInt -> CUInt -> CUInt -> IO ()
foreign import ccall "emfb_stk_twozero_tickSub" c_emfb_stk_twozero_tickSub :: TwoZeroPtr -> StkFramesPtr -> StkFramesPtr -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()

newTwoZero :: Double -> IO TwoZero
newTwoZero thePole = withCurriedStkExceptHandlingNewObject_partial (\foreignPtr -> TwoZero foreignPtr) c_emfb_stk_twozero_delete_ptr c_emfb_stk_twozero_new (\fun -> fun (CDouble thePole) )

deleteTwoZero :: TwoZero -> IO ()
deleteTwoZero = deleteStkObject twoZeroForeignPtr

createSetValue :: ( TwoZeroPtr -> CDouble -> IO () ) -> ( TwoZero -> Double -> IO () )
createSetValue = setter exceptionSafeSelfAction

twoZeroSetGain = createSetValue c_emfb_stk_twozero_setGain

twoZeroIgnoreSampleRateChange :: TwoZero -> Bool -> IO ()
twoZeroIgnoreSampleRateChange twoZero ignore = exceptionSafeSelfAction twoZero c_emfb_stk_twozero_ignoreSampleRateChange (\fun -> fun ( hsctypeconvert ignore ) )

twoZeroSetB0 = createSetValue c_emfb_stk_twozero_setB0
twoZeroSetB1 = createSetValue c_emfb_stk_twozero_setB1
twoZeroSetB2 = createSetValue c_emfb_stk_twozero_setB2

twoZeroSetCoefficients :: TwoZero -> Double -> Double -> Double -> Bool -> IO ()
twoZeroSetCoefficients twoZero b0 b1 b2 clearState = exceptionSafeSelfAction twoZero c_emfb_stk_twozero_setCoefficients (\fun -> fun ( CDouble b0 ) ( CDouble b1 ) ( CDouble b2 ) ( hsctypeconvert clearState ) )

twoZeroSetNotch :: TwoZero -> Double -> Double -> IO ()
twoZeroSetNotch twoZero frequency radius = exceptionSafeSelfAction twoZero c_emfb_stk_twozero_setNotch (\fun -> fun ( CDouble frequency ) ( CDouble radius ) )

twoZeroTickInplace :: TwoZero -> StkFrames -> Word32 -> IO ()
twoZeroTickInplace = createStkFramesTickInplaceFun exceptionSafeSelfAction c_emfb_stk_twozero_tickInplace

twoZeroTick :: TwoZero -> StkFrames -> StkFrames -> Word32 -> Word32 -> IO ()
twoZeroTick = createStkFramesTickFun exceptionSafeSelfAction c_emfb_stk_twozero_tick

twozeroTickSubInplace :: TwoZero -> StkFrames -> Word32 -> Word32 -> Word32 -> IO ()
twozeroTickSubInplace = createStkFramesTickSubInplaceFun exceptionSafeSelfAction c_emfb_stk_twozero_tickSubInplace

twozeroTickSub :: TwoZero -> StkFrames -> StkFrames -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
twozeroTickSub = createStkFramesTickSubFun exceptionSafeSelfAction c_emfb_stk_twozero_tickSub
