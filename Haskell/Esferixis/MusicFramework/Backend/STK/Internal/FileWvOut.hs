module Esferixis.MusicFramework.Backend.STK.Internal.FileWvOut where

import Esferixis.MusicFramework.Backend.STK.Internal.Misc
import Esferixis.MusicFramework.Backend.STK.Internal.Frames

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

data NativeFileWvOut
type FileWvOutPtr = Ptr NativeFileWvOut

foreign import ccall "emfb_stk_filewvout_create" c_emfb_stk_filewvout_create :: Ptr CString -> CString -> CInt -> CLong -> CLong -> CInt -> IO FileWvOutPtr
foreign import ccall "emfb_stk_filewvout_delete" c_emfb_stk_filewvout_delete :: Ptr CString -> FileWvOutPtr -> IO ()
foreign import ccall "emfb_stk_filewvout_tick" c_emfb_stk_filewvout_tick :: Ptr CString -> FileWvOutPtr -> FramesPtr -> IO ()
