module Esferixis.MusicFramework.Backend.STK ( FileWrite, FileWvOut, Guitar ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

import Esferixis.MusicFramework.Backend.STK.Internal.Misc
import Esferixis.MusicFramework.Backend.STK.Internal.Filewrite
import Esferixis.MusicFramework.Backend.STK.Internal.Frames
import Esferixis.MusicFramework.Backend.STK.Internal.FileWvOut
import Esferixis.MusicFramework.Backend.STK.Internal.Guitar

import Control.Exception
import Data.Typeable

data StkException = StkException String
   deriving Typeable
instance Show StkException where
   show (StkException message) = message

{-
handleStkException :: Ptr ExceptDescPtr -> IO ()
handleStkException nativeExcept fun = do
   exceptionDescPtr <- 
-}

class StkValue a where
   cvalue :: a -> CInt

data FileWrite = FileWav deriving Show
instance StkValue FileWrite where
   cvalue FileWav = c_emfb_stk_filewrite_FILEWAV

data FileWvOut = FileWvOutPtr

data Guitar = GuitarPtr
