module Esferixis.MusicFramework.Backend.STK.Internal.Misc where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

data ExceptDescPtr = Ptr CString
