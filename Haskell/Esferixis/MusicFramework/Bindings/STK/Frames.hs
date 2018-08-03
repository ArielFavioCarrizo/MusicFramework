{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Bindings.STK.Frames
   ( StkFrames
   , StkFramesPtr
   , NativeStkFrames
   , StkChannelFrames( StkChannelFrames, stkChannelFrames_frames, stkChannelFrames_nChannel )
   , newZeroedStkFrames
   , newValuedStkFrames
   , withStkFramesPtr
   , stkFramesChannels
   , stkFramesLength
   , stkFramesClone
   , stkFramesAdd
   , stkFramesMulHomologs ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.ForeignPtr

import Data.Word

import Esferixis.MusicFramework.Bindings.STK
import Esferixis.MusicFramework.Bindings.STK.Internal.Misc

data NativeStkFrames
type StkFramesPtr = Ptr NativeStkFrames

foreign import ccall "emfb_stk_stkframes_new_zero" c_emfb_stk_frames_new_zero :: Ptr ExceptDescPtr -> CUInt -> CUInt -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_new_valued" c_emfb_stk_frames_new_valued :: Ptr ExceptDescPtr -> CDouble -> CUInt -> CUInt -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_channels" c_emfb_stk_frames_channels :: StkFramesPtr -> IO CUInt
foreign import ccall "emfb_stk_stkframes_nFrames" c_emfb_stk_stkframes_nFrames :: StkFramesPtr -> IO CUInt
foreign import ccall "emfb_stk_stkframes_clone" c_emfb_stk_stkframes_clone :: Ptr ExceptDescPtr -> StkFramesPtr -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_add" c_emfb_stk_stkframes_add :: Ptr ExceptDescPtr -> StkFramesPtr -> StkFramesPtr -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_mulHomologs" c_emfb_stk_stkframes_mulHomologs :: Ptr ExceptDescPtr -> StkFramesPtr -> StkFramesPtr -> IO StkFramesPtr

foreign import ccall "&emfb_stk_stkframes_delete" c_emfb_stk_frames_delete_ptr :: FunPtr ( Ptr NativeStkFrames -> IO () )

data StkFrames = StkFrames ( ForeignPtr NativeStkFrames )
framesForeignPtr (StkFrames a) = a

data StkChannelFrames = StkChannelFrames { stkChannelFrames_frames :: StkFrames
                                         , stkChannelFrames_nChannel :: Word32
                                         }

stkFramesPureBinaryOp nativeFun stkFrames1 stkFrames2 = withStkFramesPtr stkFrames1 (\c_stkFrames1Ptr -> withStkFramesPtr stkFrames2 (\c_stkFrames2Ptr -> newStkFrames nativeFun (\fun -> fun c_stkFrames1Ptr c_stkFrames2Ptr ) ) )

newStkFrames = withCurriedStkExceptHandlingNewObject_partial (\foreignPtr -> StkFrames foreignPtr) c_emfb_stk_frames_delete_ptr
unhandledFramesAction = exceptionSafeStkObjectAction framesForeignPtr
exceptHandledFramesAction frames nativeFun actionFun = ( withCurriedStkExceptHandlingObjectAction framesForeignPtr nativeFun actionFun ) frames

newZeroedStkFrames :: Word32 -> Word32 -> IO StkFrames
newZeroedStkFrames nFrames nChannels = newStkFrames c_emfb_stk_frames_new_zero (\fun -> fun ( CUInt nFrames ) ( CUInt nChannels ) )

newValuedStkFrames :: Double -> Word32 -> Word32 -> IO StkFrames
newValuedStkFrames value nFrames nChannels = newStkFrames c_emfb_stk_frames_new_valued (\fun -> fun (CDouble value) ( CUInt nFrames) (CUInt nChannels ) )

withStkFramesPtr = withStkObjectPtr framesForeignPtr

stkFramesChannels :: StkFrames -> IO Word32
stkFramesChannels = unhandledFramesAction c_emfb_stk_frames_channels (\fun -> do
   c_channels <- fun
   return (fromIntegral c_channels) )

stkFramesLength :: StkFrames -> IO Word32
stkFramesLength = unhandledFramesAction c_emfb_stk_stkframes_nFrames (\fun -> do
   c_nFrames <- fun
   return (fromIntegral c_nFrames) )

stkFramesClone :: StkFrames -> IO StkFrames
stkFramesClone stkFrames = withStkFramesPtr stkFrames (\c_stkFramesPtr -> newStkFrames c_emfb_stk_stkframes_clone (\fun -> fun c_stkFramesPtr ) )

stkFramesAdd :: StkFrames -> StkFrames -> IO StkFrames
stkFramesAdd = stkFramesPureBinaryOp c_emfb_stk_stkframes_add

stkFramesMulHomologs :: StkFrames -> StkFrames -> IO StkFrames
stkFramesMulHomologs = stkFramesPureBinaryOp c_emfb_stk_stkframes_mulHomologs
