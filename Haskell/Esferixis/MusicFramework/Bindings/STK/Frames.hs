{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Bindings.STK.Frames
   ( newZeroedStkFrames
   , newValuedStkFrames
   , channelsStkFrames
   , withStkFramesPtr
   , StkFrames
   , StkChannelFrames( StkChannelFrames, stkChannelFrames_frames, stkChannelFrames_nChannel )
   , StkFramesPtr
   , NativeStkFrames ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.ForeignPtr

import Data.Word

import Esferixis.MusicFramework.Bindings.STK
import Esferixis.MusicFramework.Bindings.STK.Internal.Misc

data NativeStkFrames
type StkFramesPtr = Ptr NativeStkFrames

foreign import ccall "emfb_stk_stkframes_new_zero" c_emfb_stk_frames_new_zero :: Ptr CString -> CUInt -> CUInt -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_new_valued" c_emfb_stk_frames_new_valued :: Ptr CString -> CDouble -> CUInt -> CUInt -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_channels" c_emfb_stk_frames_channels :: StkFramesPtr -> IO CUInt
foreign import ccall "&emfb_stk_stkframes_delete" c_emfb_stk_frames_delete_ptr :: FunPtr ( Ptr NativeStkFrames -> IO () )

data StkFrames = StkFrames ( ForeignPtr NativeStkFrames )
framesForeignPtr (StkFrames a) = a

data StkChannelFrames = StkChannelFrames { stkChannelFrames_frames :: StkFrames
                                         , stkChannelFrames_nChannel :: Word32
                                         }

newStkFrames = withCurriedStkExceptHandlingNewObject_partial (\foreignPtr -> StkFrames foreignPtr) c_emfb_stk_frames_delete_ptr
unhandledFramesAction = exceptionSafeStkObjectAction framesForeignPtr

newZeroedStkFrames :: Word32 -> Word32 -> IO StkFrames
newZeroedStkFrames nFrames nChannels = newStkFrames c_emfb_stk_frames_new_zero (\fun -> fun ( CUInt nFrames ) ( CUInt nChannels ) )

newValuedStkFrames :: Double -> Word32 -> Word32 -> IO StkFrames
newValuedStkFrames value nFrames nChannels = newStkFrames c_emfb_stk_frames_new_valued (\fun -> fun (CDouble value) ( CUInt nFrames) (CUInt nChannels ) )

channelsStkFrames :: StkFrames -> IO Word32
channelsStkFrames = unhandledFramesAction c_emfb_stk_frames_channels (\fun -> do
   c_channels <- fun
   return (fromIntegral c_channels) )

withStkFramesPtr :: StkFrames -> ( Ptr NativeStkFrames -> IO a ) -> IO a
withStkFramesPtr frames fun = withForeignPtr ( framesForeignPtr frames ) fun
