{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Bindings.STK.OnePole
   ( OnePole
   , newOnePole
   , deleteOnePole
   , onePoleSetGain
   , onePoleSetB0
   , onePoleSetA1
   , onePoleSetCoefficients
   , onePoleSetPole
   , onePoleTickInplace
   , onePoleTick ) where

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

data NativeOnePole
type OnePolePtr = Ptr NativeOnePole

data OnePole = OnePole (ForeignPtr NativeOnePole)
onePoleForeignPtr (OnePole a) = a

unhandledOnePoleAction onePole nativeFun actionFun = withForeignPtr ( onePoleForeignPtr onePole ) (\c_onePolePtr -> actionFun ( nativeFun c_onePolePtr ) )

foreign import ccall "emfb_stk_onepole_new" c_emfb_stk_onepole_new :: Ptr ExceptDescPtr -> CDouble -> IO OnePolePtr
foreign import ccall "&emfb_stk_onepole_delete" c_emfb_stk_onepole_delete_ptr :: FunPtr ( OnePolePtr -> IO () )
foreign import ccall unsafe "emfb_stk_onepole_setGain" c_emfb_stk_onepole_setGain :: OnePolePtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_onepole_setB0" c_emfb_stk_onepole_setB0 :: OnePolePtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_onepole_setA1" c_emfb_stk_onepole_setA1 :: OnePolePtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_onepole_setCoefficients" c_emfb_stk_onepole_setCoefficients :: OnePolePtr -> CDouble -> CDouble -> CInt -> IO ()
foreign import ccall unsafe "emfb_stk_onepole_setPole" c_emfb_stk_onepole_setPole :: OnePolePtr -> CDouble -> IO ()
foreign import ccall "emfb_stk_onepole_tickInplace" c_emfb_stk_onepole_tickInplace :: OnePolePtr -> StkFramesPtr -> CUInt -> IO ()
foreign import ccall "emfb_stk_onepole_tick" c_emfb_stk_onepole_tick :: OnePolePtr -> StkFramesPtr -> StkFramesPtr -> CUInt -> CUInt -> IO ()

newOnePole :: Double -> IO OnePole
newOnePole thePole = withCurriedStkExceptHandlingNewObject_partial (\foreignPtr -> OnePole foreignPtr) c_emfb_stk_onepole_delete_ptr c_emfb_stk_onepole_new (\fun -> fun (CDouble thePole) )

deleteOnePole :: OnePole -> IO ()
deleteOnePole = deleteStkObject onePoleForeignPtr

createSetValue :: ( OnePolePtr -> CDouble -> IO () ) -> ( OnePole -> Double -> IO () )
createSetValue = setter unhandledOnePoleAction

onePoleSetGain = createSetValue c_emfb_stk_onepole_setGain
onePoleSetB0 = createSetValue c_emfb_stk_onepole_setB0
onePoleSetA1 = createSetValue c_emfb_stk_onepole_setA1

onePoleSetCoefficients :: OnePole -> Double -> Double -> Bool -> IO ()
onePoleSetCoefficients onePole b0 a1 clearState = unhandledOnePoleAction onePole c_emfb_stk_onepole_setCoefficients (\fun -> fun ( CDouble b0 ) ( CDouble a1 ) ( hsctypeconvert clearState ) )

onePoleSetPole = createSetValue c_emfb_stk_onepole_setPole

onePoleTickInplace :: OnePole -> StkFrames -> Word32 -> IO ()
onePoleTickInplace = createStkFramesTickInplaceFun unhandledOnePoleAction c_emfb_stk_onepole_tickInplace

onePoleTick :: OnePole -> StkFrames -> StkFrames -> Word32 -> Word32 -> IO ()
onePoleTick = createStkFramesTickFun unhandledOnePoleAction c_emfb_stk_onepole_tick
