module Esferixis.MusicFramework.Backend.STK.Internal.Guitar where

import Esferixis.MusicFramework.Backend.STK.Internal.Misc
import Esferixis.MusicFramework.Backend.STK.Internal.Frames

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

data NativeGuitar
type GuitarPtr = Ptr NativeGuitar

foreign import ccall "emfb_stk_guitar_new" c_emfb_stk_guitar_new :: Ptr ExceptDescPtr -> CUInt -> CString -> IO GuitarPtr
foreign import ccall "emfb_stk_guitar_delete" c_emfb_stk_guitar_delete :: Ptr ExceptDescPtr -> GuitarPtr -> IO ()
foreign import ccall "emfb_stk_guitar_clear" c_emfb_stk_guitar_clear :: Ptr ExceptDescPtr -> GuitarPtr -> IO ()
foreign import ccall "emfb_stk_guitar_setLoopGain" c_emfb_stk_guitar_setLoopGain :: Ptr ExceptDescPtr -> GuitarPtr -> CFloat -> CInt -> IO ()
foreign import ccall "emfb_stk_guitar_setPluckPosition" c_emfb_stk_guitar_setPluckPosition :: Ptr ExceptDescPtr -> GuitarPtr -> CFloat -> CInt -> IO ()
foreign import ccall "emfb_stk_guitar_setFrequency" c_emfb_stk_guitar_setFrequency :: Ptr ExceptDescPtr -> GuitarPtr -> CFloat -> CInt -> IO ()
foreign import ccall "emfb_stk_guitar_noteOn" c_emfb_stk_guitar_noteOn :: Ptr ExceptDescPtr -> GuitarPtr -> CFloat -> CFloat -> CInt -> IO ()
foreign import ccall "emfb_stk_guitar_noteOff" c_emfb_stk_guitar_noteOff :: Ptr ExceptDescPtr -> GuitarPtr -> CFloat -> CInt -> IO ()
foreign import ccall "emfb_stk_guitar_tick" c_emfb_stk_guitar_tick :: Ptr ExceptDescPtr -> GuitarPtr -> FramesPtr -> FramesPtr -> CInt -> CInt -> IO ()
