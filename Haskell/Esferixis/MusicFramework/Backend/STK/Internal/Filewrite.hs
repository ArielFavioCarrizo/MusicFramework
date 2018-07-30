module Esferixis.MusicFramework.Backend.STK.Internal.Filewrite where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

foreign import ccall "emfb_stk_filewrite_FILEWAV" c_emfb_stk_filewrite_FILEWAV :: CLong
