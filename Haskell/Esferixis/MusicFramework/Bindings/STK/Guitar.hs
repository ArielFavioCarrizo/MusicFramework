{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Bindings.STK.Guitar
   ( Guitar
   , newGuitar
   , deleteGuitar
   , guitarClear
   , guitarSetLoopGain
   , guitarSetPluckPosition
   , guitarSetFrequency
   , guitarNoteOn
   , guitarNoteOff
   , guitarTick ) where

import Data.Word
import Data.Int

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.C.String
import Foreign.ForeignPtr

import Control.Exception

import Esferixis.MusicFramework.Bindings.STK.Internal.Misc
import Esferixis.MusicFramework.Bindings.STK.Frames

data NativeGuitar
type GuitarPtr = Ptr NativeGuitar

data Guitar = Guitar (ForeignPtr NativeGuitar)
guitarForeignPtr (Guitar a) = a

exceptHandledGuitarAction guitar nativeFun actionFun = ( withCurriedStkExceptHandlingObjectAction guitarForeignPtr nativeFun actionFun ) guitar

foreign import ccall "emfb_stk_guitar_new" c_emfb_stk_guitar_new :: Ptr ExceptDescPtr -> CUInt -> CString -> IO GuitarPtr
foreign import ccall "&emfb_stk_guitar_delete" c_emfb_stk_guitar_delete_ptr :: FunPtr ( GuitarPtr -> IO () )
foreign import ccall "emfb_stk_guitar_clear" c_emfb_stk_guitar_clear :: Ptr ExceptDescPtr -> GuitarPtr -> IO ()
foreign import ccall "emfb_stk_guitar_setLoopGain" c_emfb_stk_guitar_setLoopGain :: Ptr ExceptDescPtr -> GuitarPtr -> CDouble -> CInt -> IO ()
foreign import ccall "emfb_stk_guitar_setPluckPosition" c_emfb_stk_guitar_setPluckPosition :: Ptr ExceptDescPtr -> GuitarPtr -> CDouble -> CInt -> IO ()
foreign import ccall "emfb_stk_guitar_setFrequency" c_emfb_stk_guitar_setFrequency :: Ptr ExceptDescPtr -> GuitarPtr -> CDouble -> CUInt -> IO ()
foreign import ccall "emfb_stk_guitar_noteOn" c_emfb_stk_guitar_noteOn :: Ptr ExceptDescPtr -> GuitarPtr -> CDouble -> CDouble -> CUInt -> IO ()
foreign import ccall "emfb_stk_guitar_noteOff" c_emfb_stk_guitar_noteOff :: Ptr ExceptDescPtr -> GuitarPtr -> CDouble -> CUInt -> IO ()
foreign import ccall "emfb_stk_guitar_tick" c_emfb_stk_guitar_tick :: Ptr ExceptDescPtr -> GuitarPtr -> StkFramesPtr -> StkFramesPtr -> CUInt -> CUInt -> IO ()

newGuitar :: Word32 -> String -> IO Guitar
newGuitar nStrings bodyFile = withCurriedStkExceptHandlingNewObject_partial (\foreignPtr -> Guitar foreignPtr) c_emfb_stk_guitar_delete_ptr c_emfb_stk_guitar_new (\fun -> withCString bodyFile (\c_bodyFile -> fun (CUInt nStrings) c_bodyFile ) ) 

deleteGuitar :: Guitar -> IO ()
deleteGuitar = deleteStkObject guitarForeignPtr

guitarClear :: Guitar -> IO ()
guitarClear guitar = exceptHandledGuitarAction guitar c_emfb_stk_guitar_clear (\fun -> fun)

guitarSetLoopGain :: Guitar -> Double -> Int32 -> IO ()
guitarSetLoopGain guitar gain string = exceptHandledGuitarAction guitar c_emfb_stk_guitar_setLoopGain (\fun -> fun ( CDouble gain )  ( CInt string ) )

guitarSetPluckPosition :: Guitar -> Double -> Int32 -> IO ()
guitarSetPluckPosition guitar position string = exceptHandledGuitarAction guitar c_emfb_stk_guitar_setPluckPosition (\fun -> fun ( CDouble position ) ( CInt string ) )

guitarSetFrequency :: Guitar -> Double -> Word32 -> IO ()
guitarSetFrequency guitar frequency string = exceptHandledGuitarAction guitar c_emfb_stk_guitar_setFrequency (\fun -> fun ( CDouble frequency ) ( CUInt string ) )

guitarNoteOn :: Guitar -> Double -> Double -> Word32 -> IO ()
guitarNoteOn guitar frequency amplitude string = exceptHandledGuitarAction guitar c_emfb_stk_guitar_noteOn (\fun -> fun ( CDouble frequency ) ( CDouble amplitude ) ( CUInt string ) )

guitarNoteOff :: Guitar -> Double -> Word32 -> IO ()
guitarNoteOff guitar amplitude string = exceptHandledGuitarAction guitar c_emfb_stk_guitar_noteOff (\fun -> fun ( CDouble amplitude ) ( CUInt string ) )

guitarTick :: Guitar -> StkChannelFrames -> StkChannelFrames -> IO ()
guitarTick guitar ichannelframes ochannelframes = let iframes = ( stkChannelFrames_frames ichannelframes )
                                                      oframes = ( stkChannelFrames_frames ochannelframes )
                                                      c_ichannel = ( CUInt ( stkChannelFrames_nChannel ichannelframes ) )
                                                      c_ochannel = ( CUInt ( stkChannelFrames_nChannel ochannelframes ) )
                                                  in withStkFramesPtr iframes (\c_iframes -> withStkFramesPtr oframes (\c_oframes -> exceptHandledGuitarAction guitar c_emfb_stk_guitar_tick (\fun -> fun c_iframes c_oframes c_ichannel c_ochannel ) ) )
