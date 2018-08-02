{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Backend.STK.FileWvOut
   ( FileWvOut
   , createFileWvOut ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

import Control.Exception

import Data.Typeable
import Data.Int

import Esferixis.MusicFramework.Backend.STK
import Esferixis.MusicFramework.Backend.STK.Internal.Misc
import Esferixis.MusicFramework.Backend.STK.Frames

import Esferixis.MusicFramework.Backend.STK.Filewrite

import Foreign.Marshal.Alloc
import Foreign.Storable

import Foreign.ForeignPtr
import Foreign.Concurrent

data NativeFileWvOut
type FileWvOutPtr = Ptr NativeFileWvOut

foreign import ccall "emfb_stk_filewvout_create" c_emfb_stk_filewvout_create :: Ptr CString -> CString -> CInt -> CLong -> CLong -> CInt -> IO FileWvOutPtr
foreign import ccall "emfb_stk_filewvout_tick" c_emfb_stk_filewvout_tick :: Ptr CString -> FileWvOutPtr -> FramesPtr -> IO ()
foreign import ccall "emfb_stk_filewvout_delete" c_emfb_stk_filewvout_delete :: Ptr CString -> FileWvOutPtr -> IO ()

data FileWvOut = FileWvOut (ForeignPtr NativeFileWvOut)
stkForeignPtr (FileWvOut a) = a

createFileWvOut :: String -> Int32 -> FileType -> StkFormat -> Int32 -> IO FileWvOut
createFileWvOut fileName nChannels fileType format bufferFrames = do
   c_fileName <- newCString fileName
   fileWvOutPtr_raw <- handleStkExcept (\exceptDescCStrPtr -> c_emfb_stk_filewvout_create exceptDescCStrPtr c_fileName (CInt nChannels) ( cvalue fileType ) ( cvalue format ) ( CInt bufferFrames ) ) `finally` free c_fileName
   fileWvOutPtr <- Foreign.Concurrent.newForeignPtr fileWvOutPtr_raw ( deleteFileWvOut_raw fileWvOutPtr_raw )
   return ( FileWvOut fileWvOutPtr )

closeFileWvOut :: FileWvOut -> IO ()
closeFileWvOut fileWvOut = finalizeForeignPtr ( stkForeignPtr fileWvOut )

deleteFileWvOut_raw :: Ptr NativeFileWvOut -> IO ()
deleteFileWvOut_raw fileWvOutPtr = do
   handleStkExcept (\exceptDescCStrPtr -> c_emfb_stk_filewvout_delete exceptDescCStrPtr fileWvOutPtr )

