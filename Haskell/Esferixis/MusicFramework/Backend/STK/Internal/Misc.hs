module Esferixis.MusicFramework.Backend.STK.Internal.Misc
   ( ExceptDescPtr
   , handleStkExcept ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.ForeignPtr

import Esferixis.MusicFramework.Backend.STK

import Control.Exception

type ExceptDescPtr = CString

handleStkExcept :: ( Ptr CString -> IO a ) -> IO a
handleStkExcept fun = do
   alloca $ \exceptDescCStrPtr -> do
       funret <- fun exceptDescCStrPtr
       exceptDescCStr <- peek exceptDescCStrPtr
       if exceptDescCStr == nullPtr
          then return funret
          else do
             exceptDescStr <- peekCString exceptDescCStr
             throwIO ( StkException exceptDescStr )
