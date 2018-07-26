module Esferixis.MusicFramework.Backend.STK.Native where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

data FileWrite = FileWav

data EmfbStk = EmfbStk { fileWrite :: FileWrite
                       , newFileWvOut :: IO FileWvOut
                       , newFrames :: IO Frames
                       , newGuitar :: IO Guitar
                       }

data FileWvOut = FileWvOut { tick :: IO Frames -> IO () }

data Frames = Frames { IO channels :: Int )

data Guitar = Guitar { clear :: IO ()
                     , setLoopGain :: CFloat -> CUInt -> IO ()
                     , setPluckPosition :: CFloat -> CUInt -> IO ()
                     , setFrequency :: CFloat -> CUInt -> IO ()
                     , noteOn :: CFloat -> CFloat -> CUInt -> IO ()
                     , noteOff :: CFloat -> CUInt -> IO ()
                     , tick :: Frames -> Frames -> CUInt -> CUInt -> IO ()
                     }

emfbPath = "C:\\Users\\arifa\\Documents\\github\\esferixis\\esferixis-MusicFramework\\EsferixisMusicFrameworkBackend\\x64\\Release\\EMFB_STK.dll"






