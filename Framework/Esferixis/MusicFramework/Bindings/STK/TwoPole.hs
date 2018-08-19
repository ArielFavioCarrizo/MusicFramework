{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Bindings.STK.TwoPole
   ( TwoPole
   , newTwoPole
   , deleteTwoPole
   , twoPoleSetGain
   , twoPoleIgnoreSampleRateChange
   , twoPoleSetB0
   , twoPoleSetA1
   , twoPoleSetA2
   , twoPoleSetCoefficients
   , twoPoleSetResonance
   , twoPoleTickInplace
   , twoPoleTick
   , twopoleTickSubInplace
   , twopoleTickSub ) where

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

data NativeTwoPole
type TwoPolePtr = Ptr NativeTwoPole

data TwoPole = TwoPole (ForeignPtr NativeTwoPole)
twoPoleForeignPtr (TwoPole a) = a

exceptionSafeSelfAction twoPole nativeFun actionFun = withForeignPtr ( twoPoleForeignPtr twoPole ) (\c_twoPolePtr -> actionFun ( nativeFun c_twoPolePtr ) )

foreign import ccall "emfb_stk_twopole_new" c_emfb_stk_twopole_new :: Ptr ExceptDescPtr -> CDouble -> IO TwoPolePtr
foreign import ccall "&emfb_stk_twopole_delete" c_emfb_stk_twopole_delete_ptr :: FunPtr ( TwoPolePtr -> IO () )
foreign import ccall unsafe "emfb_stk_twopole_setGain" c_emfb_stk_twopole_setGain :: TwoPolePtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_twopole_ignoreSampleRateChange" c_emfb_stk_twopole_ignoreSampleRateChange :: TwoPolePtr -> CInt -> IO ()
foreign import ccall unsafe "emfb_stk_twopole_setB0" c_emfb_stk_twopole_setB0 :: TwoPolePtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_twopole_setA1" c_emfb_stk_twopole_setA1 :: TwoPolePtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_twopole_setA2" c_emfb_stk_twopole_setA2 :: TwoPolePtr -> CDouble -> IO ()
foreign import ccall unsafe "emfb_stk_twopole_setCoefficients" c_emfb_stk_twopole_setCoefficients :: TwoPolePtr -> CDouble -> CDouble -> CDouble -> CInt -> IO ()
foreign import ccall unsafe "emfb_stk_twopole_setResonance" c_emfb_stk_twopole_setResonance :: TwoPolePtr -> CDouble -> CDouble -> CInt -> IO ()
foreign import ccall "emfb_stk_twopole_tickInplace" c_emfb_stk_twopole_tickInplace :: TwoPolePtr -> StkFramesPtr -> CUInt -> IO ()
foreign import ccall "emfb_stk_twopole_tick" c_emfb_stk_twopole_tick :: TwoPolePtr -> StkFramesPtr -> StkFramesPtr -> CUInt -> CUInt -> IO ()
foreign import ccall "emfb_stk_twopole_tickSubInplace" c_emfb_stk_twopole_tickSubInplace :: TwoPolePtr -> StkFramesPtr -> CUInt -> CUInt -> CUInt -> IO ()
foreign import ccall "emfb_stk_twopole_tickSub" c_emfb_stk_twopole_tickSub :: TwoPolePtr -> StkFramesPtr -> StkFramesPtr -> CUInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()

newTwoPole :: Double -> IO TwoPole
newTwoPole thePole = withCurriedStkExceptHandlingNewObject_partial (\foreignPtr -> TwoPole foreignPtr) c_emfb_stk_twopole_delete_ptr c_emfb_stk_twopole_new (\fun -> fun (CDouble thePole) )

deleteTwoPole :: TwoPole -> IO ()
deleteTwoPole = deleteStkObject twoPoleForeignPtr

createSetValue :: ( TwoPolePtr -> CDouble -> IO () ) -> ( TwoPole -> Double -> IO () )
createSetValue = setter exceptionSafeSelfAction

twoPoleSetGain = createSetValue c_emfb_stk_twopole_setGain

twoPoleIgnoreSampleRateChange :: TwoPole -> Bool -> IO ()
twoPoleIgnoreSampleRateChange twoPole ignore = exceptionSafeSelfAction twoPole c_emfb_stk_twopole_ignoreSampleRateChange (\fun -> fun ( hsctypeconvert ignore ) )

twoPoleSetB0 = createSetValue c_emfb_stk_twopole_setB0
twoPoleSetA1 = createSetValue c_emfb_stk_twopole_setA1
twoPoleSetA2 = createSetValue c_emfb_stk_twopole_setA2

twoPoleSetCoefficients :: TwoPole -> Double -> Double -> Double -> Bool -> IO ()
twoPoleSetCoefficients twoPole b0 a1 a2 clearState = exceptionSafeSelfAction twoPole c_emfb_stk_twopole_setCoefficients (\fun -> fun ( CDouble b0 ) ( CDouble a1 ) ( CDouble a2 ) ( hsctypeconvert clearState ) )

twoPoleSetResonance :: TwoPole -> Double -> Double -> Bool -> IO ()
twoPoleSetResonance twoPole frequency radius normalise = exceptionSafeSelfAction twoPole c_emfb_stk_twopole_setResonance (\fun -> fun ( CDouble frequency ) ( CDouble radius ) ( hsctypeconvert normalise ) )

twoPoleTickInplace :: TwoPole -> StkFrames -> Word32 -> IO ()
twoPoleTickInplace = createStkFramesTickInplaceFun exceptionSafeSelfAction c_emfb_stk_twopole_tickInplace

twoPoleTick :: TwoPole -> StkFrames -> StkFrames -> Word32 -> Word32 -> IO ()
twoPoleTick = createStkFramesTickFun exceptionSafeSelfAction c_emfb_stk_twopole_tick

twopoleTickSubInplace :: TwoPole -> StkFrames -> Word32 -> Word32 -> Word32 -> IO ()
twopoleTickSubInplace = createStkFramesTickSubInplaceFun exceptionSafeSelfAction c_emfb_stk_twopole_tickSubInplace

twopoleTickSub :: TwoPole -> StkFrames -> StkFrames -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
twopoleTickSub = createStkFramesTickSubFun exceptionSafeSelfAction c_emfb_stk_twopole_tickSub
