{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Backend.STK.Filewrite
   ( FileType(FileWav) ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

import Esferixis.MusicFramework.Backend.STK

foreign import ccall "emfb_stk_filewrite_FILEWAV" c_emfb_stk_filewrite_FILEWAV :: CLong

data FileType = FileWav deriving Show
instance StkValue FileType CLong where
   cvalue FileWav = c_emfb_stk_filewrite_FILEWAV
