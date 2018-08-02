{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Backend.STK.Guitar
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

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Concurrent

import Control.Exception

import Esferixis.MusicFramework.Backend.STK.Internal.Misc
import Esferixis.MusicFramework.Backend.STK.Frames

import Data.Word

data NativeGuitar
type GuitarPtr = Ptr NativeGuitar

data Guitar = Guitar (ForeignPtr NativeGuitar)
guitarForeignPtr (Guitar a) = a

withGuitarPtr :: Guitar -> (Ptr ExceptDescPtr -> Ptr NativeGuitar -> IO a ) -> IO a
withGuitarPtr guitar fun = withForeignPtr ( guitarForeignPtr guitar ) (\c_guitarPtr -> handleStkExcept (\c_exceptDescPtr -> fun c_exceptDescPtr c_guitarPtr) )

withCurriedNativeGuitarFun guitar nativeFun actionFun = withGuitarPtr guitar (\c_exceptDescPtr c_guitarPtr -> actionFun ( nativeFun c_exceptDescPtr c_guitarPtr ) )

foreign import ccall "emfb_stk_guitar_new" c_emfb_stk_guitar_new :: Ptr ExceptDescPtr -> CUInt -> CString -> IO GuitarPtr
foreign import ccall "emfb_stk_guitar_delete" c_emfb_stk_guitar_delete :: Ptr ExceptDescPtr -> GuitarPtr -> IO ()
foreign import ccall "emfb_stk_guitar_clear" c_emfb_stk_guitar_clear :: Ptr ExceptDescPtr -> GuitarPtr -> IO ()
foreign import ccall "emfb_stk_guitar_setLoopGain" c_emfb_stk_guitar_setLoopGain :: Ptr ExceptDescPtr -> GuitarPtr -> CFloat -> CUInt -> IO ()
foreign import ccall "emfb_stk_guitar_setPluckPosition" c_emfb_stk_guitar_setPluckPosition :: Ptr ExceptDescPtr -> GuitarPtr -> CFloat -> CUInt -> IO ()
foreign import ccall "emfb_stk_guitar_setFrequency" c_emfb_stk_guitar_setFrequency :: Ptr ExceptDescPtr -> GuitarPtr -> CFloat -> CUInt -> IO ()
foreign import ccall "emfb_stk_guitar_noteOn" c_emfb_stk_guitar_noteOn :: Ptr ExceptDescPtr -> GuitarPtr -> CFloat -> CFloat -> CUInt -> IO ()
foreign import ccall "emfb_stk_guitar_noteOff" c_emfb_stk_guitar_noteOff :: Ptr ExceptDescPtr -> GuitarPtr -> CFloat -> CUInt -> IO ()
foreign import ccall "emfb_stk_guitar_tick" c_emfb_stk_guitar_tick :: Ptr ExceptDescPtr -> GuitarPtr -> StkFramesPtr -> StkFramesPtr -> CUInt -> CUInt -> IO ()

newGuitar :: Word32 -> String -> IO Guitar
newGuitar nStrings bodyFile = do
   let c_nStrings = CUInt nStrings
   c_bodyFile <- newCString bodyFile
   c_guitarPtr <- handleStkExcept (\c_exceptDescPtr -> c_emfb_stk_guitar_new c_exceptDescPtr c_nStrings c_bodyFile) `finally` free c_bodyFile
   guitarForeignPtr <- Foreign.Concurrent.newForeignPtr c_guitarPtr ( deleteGuitar_raw c_guitarPtr )
   return ( Guitar guitarForeignPtr )

deleteGuitar_raw :: GuitarPtr -> IO ()
deleteGuitar_raw guitarPtr = do
   handleStkExcept (\c_exceptDescPtr -> c_emfb_stk_guitar_delete c_exceptDescPtr guitarPtr )

deleteGuitar :: Guitar -> IO ()
deleteGuitar guitar = finalizeForeignPtr ( guitarForeignPtr guitar )

guitarClear :: Guitar -> IO ()
guitarClear guitar = withGuitarPtr guitar (\c_exceptDescPtr c_guitarPtr -> c_emfb_stk_guitar_clear c_exceptDescPtr c_guitarPtr)

guitarSetLoopGain :: Guitar -> Float -> Word32 -> IO ()
guitarSetLoopGain guitar gain string = withGuitarPtr guitar (\c_exceptDescPtr c_guitarPtr -> c_emfb_stk_guitar_setLoopGain c_exceptDescPtr c_guitarPtr ( CFloat gain )  ( CUInt string ) )

guitarSetPluckPosition :: Guitar -> Float -> Word32 -> IO ()
guitarSetPluckPosition guitar position string = withCurriedNativeGuitarFun guitar c_emfb_stk_guitar_setLoopGain (\fun -> fun ( CFloat position ) ( CUInt string ) )

guitarSetFrequency :: Guitar -> Float -> Word32 -> IO ()
guitarSetFrequency guitar frequency string = withCurriedNativeGuitarFun guitar c_emfb_stk_guitar_setFrequency (\fun -> fun ( CFloat frequency ) ( CUInt string ) )

guitarNoteOn :: Guitar -> Float -> Float -> Word32 -> IO ()
guitarNoteOn guitar frequency amplitude string = withCurriedNativeGuitarFun guitar c_emfb_stk_guitar_noteOn (\fun -> fun ( CFloat frequency ) ( CFloat amplitude ) ( CUInt string ) )

guitarNoteOff :: Guitar -> Float -> Word32 -> IO ()
guitarNoteOff guitar amplitude string = withCurriedNativeGuitarFun guitar c_emfb_stk_guitar_noteOff (\fun -> fun ( CFloat amplitude ) ( CUInt string ) )

guitarTick :: Guitar -> StkChannelFrames -> StkChannelFrames -> IO ()
guitarTick guitar ichannelframes ochannelframes = let iframes = ( stkChannelFrames_frames ichannelframes )
                                                      oframes = ( stkChannelFrames_frames ochannelframes )
                                                      c_ichannel = ( CUInt ( stkChannelFrames_nChannel ichannelframes ) )
                                                      c_ochannel = ( CUInt ( stkChannelFrames_nChannel ochannelframes ) )
                                                  in withStkFramesPtr iframes (\c_iframes -> withStkFramesPtr oframes (\c_oframes -> withCurriedNativeGuitarFun guitar c_emfb_stk_guitar_tick (\fun -> fun c_iframes c_oframes c_ichannel c_ochannel ) ) )
