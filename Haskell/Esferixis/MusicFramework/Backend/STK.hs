{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Backend.STK
   ( StkFormat (StkSInt16)
   , FileType (FileWav)
   , FileWvOut, Guitar ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc
import Foreign.Storable

import Esferixis.MusicFramework.Backend.STK.Internal.Misc
import Esferixis.MusicFramework.Backend.STK.Internal.Filewrite
import Esferixis.MusicFramework.Backend.STK.Internal.Frames
import Esferixis.MusicFramework.Backend.STK.Internal.FileWvOut
import Esferixis.MusicFramework.Backend.STK.Internal.Guitar

import Control.Exception
import Data.Typeable
import Data.Int

data StkException = StkException String
   deriving Typeable
instance Show StkException where
   show (StkException message) = message
instance Exception StkException

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

class StkValue a b where
   cvalue :: a -> b

data StkFormat = StkSInt16 deriving Show
instance StkValue StkFormat CLong where
   cvalue StkSInt16 = c_emfb_stk_sint16

data FileType = FileWav deriving Show
instance StkValue FileType CLong where
   cvalue FileWav = c_emfb_stk_filewrite_FILEWAV

class StkPtr a b where
   cptr :: a -> Ptr b

data FileWvOut = FileWvOut (Ptr NativeFileWvOut)
instance StkPtr FileWvOut NativeFileWvOut where
   cptr ( FileWvOut a ) = a

createFileWvOut :: String -> Int32 -> FileType -> StkFormat -> Int32 -> IO FileWvOut
createFileWvOut fileName nChannels fileType format bufferFrames = do
   c_fileName <- newCString fileName
   fileWvOutPtr <- handleStkExcept (\exceptDescCStrPtr -> c_emfb_stk_filewvout_create exceptDescCStrPtr c_fileName (CInt nChannels) ( cvalue fileType ) ( cvalue format ) ( CInt bufferFrames ) ) `finally` free c_fileName
   return ( FileWvOut fileWvOutPtr )

deleteFileWvOut :: FileWvOut -> IO ()
deleteFileWvOut fileWvOut = do
   handleStkExcept (\exceptDescCStrPtr -> c_emfb_stk_filewvout_delete exceptDescCStrPtr ( cptr fileWvOut ) )

data Guitar = GuitarPtr
