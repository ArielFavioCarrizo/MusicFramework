module Esferixis.MusicFramework.Bindings.STK.Internal.Misc
   ( ExceptDescPtr
   , handleStkExcept
   , withCurriedStkExceptHandling
   , withCurriedStkExceptHandlingNewObject_partial
   , withCurriedStkExceptHandlingObjectAction
   , exceptionSafeStkObjectAction
   , deleteStkObject ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr, FunPtr)
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

withCurriedStkExceptHandling nativeFun actionFun = handleStkExcept (\c_exceptDescPtr -> actionFun ( nativeFun c_exceptDescPtr ) )

withCurriedStkExceptHandlingNewObject_partial wrappedObjectFromForeignPtr nativeDeletePtr nativeNew actionFun = do
   object_rawptr <- withCurriedStkExceptHandling nativeNew ( \fun -> actionFun fun )
   object_foreignptr <- ( newForeignPtr nativeDeletePtr object_rawptr )
   return ( wrappedObjectFromForeignPtr object_foreignptr )

withCurriedStkExceptHandlingObjectAction foreignPtrFromWrappedObject nativeFun actionFun wrappedObject = withForeignPtr ( foreignPtrFromWrappedObject wrappedObject ) (\c_objectPtr -> ( withCurriedStkExceptHandling nativeFun ) (\fun -> actionFun ( fun c_objectPtr ) ) )

exceptionSafeStkObjectAction foreignPtrFromWrappedObject nativeFun actionFun wrappedObject = withForeignPtr ( foreignPtrFromWrappedObject wrappedObject ) (\c_objectPtr -> actionFun ( nativeFun c_objectPtr ) )

deleteStkObject foreignPtrFromWrappedObject wrappedObject = finalizeForeignPtr ( foreignPtrFromWrappedObject wrappedObject )
