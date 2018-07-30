module Esferixis.MusicFramework.Backend.STK.Internal.Misc where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

type ExceptDescPtr = Ptr CString

foreign import ccall "emfb_stk_sint16" c_emfb_stk_sint16 :: CLong
