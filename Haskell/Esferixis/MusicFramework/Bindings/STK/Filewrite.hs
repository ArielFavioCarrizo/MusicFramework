{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Bindings.STK.Filewrite
   ( FileType(FileWav) ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

import Esferixis.MusicFramework.Bindings.STK

foreign import ccall "emfb_stk_filewrite_FILE_WAV" c_emfb_stk_filewrite_FILEWAV :: CULong

data FileType = FileWav deriving Show
instance StkValue FileType CULong where
   cvalue FileWav = c_emfb_stk_filewrite_FILEWAV
