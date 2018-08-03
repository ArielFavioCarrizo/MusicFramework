module Esferixis.MusicFramework.Bindings.STK.Internal.Misc
   ( ExceptDescPtr
   , handleStkExcept
   , build_withObjectPtr
   , build_withCurriedNativeObjectFun ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.ForeignPtr

import Esferixis.MusicFramework.Bindings.STK

import Control.Exception

type ExceptDescPtr = CString

foreign import ccall "emfb_stk_cfree" nativeStkCFree :: Ptr a -> IO ()

handleStkExcept :: ( Ptr CString -> IO a ) -> IO a
handleStkExcept fun = do
   alloca $ \exceptDescCStrPtr -> do
       funret <- fun exceptDescCStrPtr
       exceptDescCStr <- peek exceptDescCStrPtr
       if exceptDescCStr == nullPtr
          then return funret
          else do
             exceptDescStr <- peekCString exceptDescCStr
             nativeStkCFree exceptDescCStr
             throwIO ( StkException exceptDescStr )

build_withObjectPtr :: (objectWrapper -> ForeignPtr nativeObject ) -> ( objectWrapper -> (Ptr ExceptDescPtr -> Ptr nativeObject -> IO r) -> IO r )
build_withObjectPtr objectForeignPtr = \object fun -> withForeignPtr ( objectForeignPtr object ) (\c_objectPtr -> handleStkExcept (\c_exceptDescPtr -> fun c_exceptDescPtr c_objectPtr) )

build_withCurriedNativeObjectFun withObjectPtr = \object nativeFun actionFun -> withObjectPtr object (\c_exceptDescPtr c_objectPtr -> actionFun ( nativeFun c_exceptDescPtr c_objectPtr ) )

