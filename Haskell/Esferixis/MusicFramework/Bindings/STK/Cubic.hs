{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Bindings.STK.Cubic
   ( Cubic
   , newCubic
   , deleteCubic
   , cubicSetA1
   , cubicSetA2
   , cubicSetA3
   , cubicSetGain
   , cubicSetThreshold
   , cubicTickInplace
   , cubicTick ) where

import Data.Word
import Data.Int

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.C.String
import Foreign.ForeignPtr

import Control.Exception

import Esferixis.MusicFramework.Bindings.STK.Internal.Misc
import Esferixis.MusicFramework.Bindings.STK.Frames

data NativeCubic
type CubicPtr = Ptr NativeCubic

data Cubic = Cubic (ForeignPtr NativeCubic)
cubicForeignPtr (Cubic a) = a

unhandledCubicAction cubic nativeFun actionFun = withForeignPtr ( cubicForeignPtr cubic ) (\c_cubicPtr -> actionFun ( nativeFun c_cubicPtr ) )

foreign import ccall "emfb_stk_cubic_new" c_emfb_stk_cubic_new :: Ptr ExceptDescPtr -> IO CubicPtr
foreign import ccall "&emfb_stk_cubic_delete" c_emfb_stk_cubic_delete_ptr :: FunPtr ( CubicPtr -> IO () )
foreign import ccall "emfb_stk_cubic_setA1" c_emfb_stk_cubic_setA1 :: CubicPtr -> CDouble -> IO ()
foreign import ccall "emfb_stk_cubic_setA2" c_emfb_stk_cubic_setA2 :: CubicPtr -> CDouble -> IO ()
foreign import ccall "emfb_stk_cubic_setA3" c_emfb_stk_cubic_setA3 :: CubicPtr -> CDouble -> IO ()
foreign import ccall "emfb_stk_cubic_setGain" c_emfb_stk_cubic_setGain :: CubicPtr -> CDouble -> IO ()
foreign import ccall "emfb_stk_cubic_setThreshold" c_emfb_stk_cubic_setThreshold :: CubicPtr -> CDouble -> IO ()
foreign import ccall "emfb_stk_cubic_tickInplace" c_emfb_stk_cubic_tickInplace :: CubicPtr -> StkFramesPtr -> CUInt -> IO ()
foreign import ccall "emfb_stk_cubic_tick" c_emfb_stk_cubic_tick :: CubicPtr -> StkFramesPtr -> StkFramesPtr -> CUInt -> CUInt -> IO ()

newCubic :: IO Cubic
newCubic = withCurriedStkExceptHandlingNewObject_partial (\foreignPtr -> Cubic foreignPtr) c_emfb_stk_cubic_delete_ptr c_emfb_stk_cubic_new (\fun -> fun)

deleteCubic :: Cubic -> IO ()
deleteCubic = deleteStkObject cubicForeignPtr

createSetCubicValue :: ( CubicPtr -> CDouble -> IO () ) -> ( Cubic -> Double -> IO () )
createSetCubicValue nativeFun = \cubic value -> unhandledCubicAction cubic nativeFun (\fun -> fun ( CDouble value ) )

cubicSetA1 = createSetCubicValue c_emfb_stk_cubic_setA1
cubicSetA2 = createSetCubicValue c_emfb_stk_cubic_setA2
cubicSetA3 = createSetCubicValue c_emfb_stk_cubic_setA3
cubicSetGain = createSetCubicValue c_emfb_stk_cubic_setGain
cubicSetThreshold = createSetCubicValue c_emfb_stk_cubic_setThreshold

cubicTickInplace :: Cubic -> StkFrames -> Word32 -> IO ()
cubicTickInplace = createStkFramesTickInplaceFun unhandledCubicAction c_emfb_stk_cubic_tickInplace

cubicTick :: Cubic -> StkFrames -> StkFrames -> Word32 -> Word32 -> IO ()
cubicTick = createStkFramesTickFun unhandledCubicAction c_emfb_stk_cubic_tick
