{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Bindings.STK.Frames
   ( StkFrames
   , StkFramesPtr
   , NativeStkFrames
   , StkChannelFrames( StkChannelFrames, stkChannelFrames_frames, stkChannelFrames_nChannel )
   , stkFramesCheckSameShape
   , createStkFramesTickFun
   , createStkFramesTickInplaceFun
   , createStkFramesTickSubInplaceFun
   , createStkFramesTickSubFun
   , newZeroedStkFrames
   , newValuedStkFrames
   , deleteStkFrames
   , withStkFramesPtr
   , stkFramesChannels
   , stkFramesLength
   , stkFramesClone
   , stkFramesAdd
   , stkFramesMulHomologs
   , stkFramesAddInplace
   , stkFramesMulHomologsInplace
   , stkFramesScale
   , stkFramesScaleInplace ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.ForeignPtr

import Data.Word

import Esferixis.MusicFramework.Bindings.STK
import Esferixis.MusicFramework.Bindings.STK.Internal.Misc

import Control.Exception

data NativeStkFrames
type StkFramesPtr = Ptr NativeStkFrames

foreign import ccall "emfb_stk_stkframes_new_zero" c_emfb_stk_frames_new_zero :: Ptr ExceptDescPtr -> CUInt -> CUInt -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_new_valued" c_emfb_stk_frames_new_valued :: Ptr ExceptDescPtr -> CDouble -> CUInt -> CUInt -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_channels" c_emfb_stk_frames_channels :: StkFramesPtr -> IO CUInt
foreign import ccall "emfb_stk_stkframes_nFrames" c_emfb_stk_stkframes_nFrames :: StkFramesPtr -> IO CUInt
foreign import ccall "emfb_stk_stkframes_clone" c_emfb_stk_stkframes_clone :: Ptr ExceptDescPtr -> StkFramesPtr -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_add" c_emfb_stk_stkframes_add :: Ptr ExceptDescPtr -> StkFramesPtr -> StkFramesPtr -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_mulHomologs" c_emfb_stk_stkframes_mulHomologs :: Ptr ExceptDescPtr -> StkFramesPtr -> StkFramesPtr -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_addInplace" c_emfb_stk_stkframes_addInplace :: StkFramesPtr -> StkFramesPtr -> IO ()
foreign import ccall "emfb_stk_stkframes_mulHomologsInplace" c_emfb_stk_stkframes_mulHomologsInplace :: StkFramesPtr -> StkFramesPtr -> IO ()
foreign import ccall "emfb_stk_stkframes_scale" c_emfb_stk_stkframes_scale :: Ptr ExceptDescPtr -> StkFramesPtr -> CDouble -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_scaleInplace" c_emfb_stk_stkframes_scaleInplace :: StkFramesPtr -> CDouble -> IO ()

foreign import ccall "&emfb_stk_stkframes_delete" c_emfb_stk_frames_delete_ptr :: FunPtr ( Ptr NativeStkFrames -> IO () )

data StkFrames = StkFrames { framesForeignPtr :: ForeignPtr NativeStkFrames
                           , stkFramesLength :: Word32
                           , stkFramesChannels :: Word32 }

data StkChannelFrames = StkChannelFrames { stkChannelFrames_frames :: StkFrames
                                         , stkChannelFrames_nChannel :: Word32
                                         }

stkFramesCheckChannelExists :: StkFrames -> Word32 -> IO ()
stkFramesCheckChannelExists stkFrames channel = do
   if ( ( channel >= 0 ) && ( channel <= ( stkFramesChannels stkFrames - 1 ) ) )
      then return ()
   else
      throwIO ( StkException "Invalid channel number" )

stkFramesCheckSameLength :: StkFrames -> StkFrames -> IO ()
stkFramesCheckSameLength stkFrames1 stkFrames2 = do
   if (stkFramesLength stkFrames1 ) == ( stkFramesLength stkFrames2 )
      then return ()
   else
      throwIO ( StkException "Length mismatch" )

stkFramesCheckSameChannels :: StkFrames -> StkFrames -> IO ()
stkFramesCheckSameChannels stkFrames1 stkFrames2 = do
   if ( stkFramesChannels stkFrames1 ) == ( stkFramesChannels stkFrames2 )
      then return ()
   else
      throwIO ( StkException "Channels mismatch" )

stkFramesCheckValidInterval :: StkFrames -> Word32 -> Word32 -> IO ()
stkFramesCheckValidInterval stkFrames offset length = do
   if ( ( offset + length ) <= ( stkFramesLength stkFrames ) )
      then return ()
   else
      throwIO ( StkException "Invalid interval" )

stkFramesCheckSameShape :: StkFrames -> StkFrames -> IO ()
stkFramesCheckSameShape stkFrames1 stkFrames2 = do
   stkFramesCheckSameLength stkFrames1 stkFrames2
   stkFramesCheckSameChannels stkFrames1 stkFrames2

createStkFramesTickInplaceFun doUnhandledObjectAction nativeFun = \object frames channel -> do
   stkFramesCheckChannelExists frames channel
   withStkFramesPtr frames $ \c_frames -> doUnhandledObjectAction object nativeFun $ \fun -> fun c_frames (CUInt channel)

createStkFramesTickFun doUnhandledObjectAction nativeFun = \object iframes oframes ichannel ochannel -> do
   stkFramesCheckSameLength iframes oframes
   stkFramesCheckChannelExists iframes ichannel
   stkFramesCheckChannelExists oframes ochannel
   withStkFramesPtr iframes $ \c_iframes -> withStkFramesPtr oframes $ \c_oframes -> doUnhandledObjectAction object nativeFun $ \fun -> fun c_iframes c_oframes (CUInt ichannel) (CUInt ochannel)

createStkFramesTickSubInplaceFun doUnhandledObjectAction nativeFun = \object frames offset length channel -> do
   stkFramesCheckChannelExists frames channel
   stkFramesCheckValidInterval frames offset length
   withStkFramesPtr frames $ \c_frames -> doUnhandledObjectAction object nativeFun $ \fun -> fun c_frames (CUInt offset) (CUInt length) (CUInt channel)

createStkFramesTickSubFun doUnhandledObjectAction nativeFun = \object iFrames oFrames iOffset oOffset length iChannel oChannel -> do
   stkFramesCheckChannelExists iFrames iChannel
   stkFramesCheckChannelExists oFrames oChannel
   stkFramesCheckValidInterval iFrames iOffset length
   stkFramesCheckValidInterval oFrames oOffset length
   withStkFramesPtr iFrames $ \c_iframes -> withStkFramesPtr oFrames $ \c_oframes -> doUnhandledObjectAction object nativeFun $ \fun -> fun c_iframes c_oframes (CUInt iOffset) (CUInt oOffset) (CUInt length) (CUInt iChannel) (CUInt oChannel)

newStkFramesFromSourceAndForeignPtr sourceStkFrames newStkFramesForeignPtr = StkFrames newStkFramesForeignPtr ( stkFramesLength sourceStkFrames ) ( stkFramesChannels sourceStkFrames )

newStkFramesFromExisting sourceFrames = withCurriedStkExceptHandlingNewObject_partial (\foreignPtr -> newStkFramesFromSourceAndForeignPtr sourceFrames foreignPtr) c_emfb_stk_frames_delete_ptr

newStkFramesOriginal nFrames nChannels = withCurriedStkExceptHandlingNewObject_partial (\foreignPtr -> StkFrames foreignPtr nFrames nChannels ) c_emfb_stk_frames_delete_ptr

unhandledFramesAction = exceptionSafeStkObjectAction framesForeignPtr
exceptHandledFramesAction frames nativeFun actionFun = exceptionUnsafeStkObjectAction framesForeignPtr frames nativeFun actionFun

stkFramesPureBinaryOp nativeFun stkFrames1 stkFrames2 = do
   stkFramesCheckSameShape stkFrames1 stkFrames2
   withStkFramesPtr stkFrames1 $ \c_stkFrames1Ptr -> withStkFramesPtr stkFrames2 $ \c_stkFrames2Ptr -> newStkFramesFromExisting stkFrames2 nativeFun $ \fun -> fun c_stkFrames1Ptr c_stkFrames2Ptr

stkFramesImplaceBinaryOp nativeFun stkFrames1 stkFrames2 = do
   stkFramesCheckSameShape stkFrames1 stkFrames2
   withStkFramesPtr stkFrames1 $ \c_stkFrames1 -> withStkFramesPtr stkFrames2 $ \c_stkFrames2 -> nativeFun c_stkFrames1 c_stkFrames2

newZeroedStkFrames :: Word32 -> Word32 -> IO StkFrames
newZeroedStkFrames nFrames nChannels = newStkFramesOriginal nFrames nChannels c_emfb_stk_frames_new_zero $ \fun -> fun ( CUInt nFrames ) ( CUInt nChannels )

newValuedStkFrames :: Double -> Word32 -> Word32 -> IO StkFrames
newValuedStkFrames value nFrames nChannels = newStkFramesOriginal nFrames nChannels c_emfb_stk_frames_new_valued $ \fun -> fun (CDouble value) ( CUInt nFrames) (CUInt nChannels )

deleteStkFrames :: StkFrames -> IO ()
deleteStkFrames = deleteStkObject framesForeignPtr

withStkFramesPtr = withStkObjectPtr framesForeignPtr

stkFramesClone :: StkFrames -> IO StkFrames
stkFramesClone stkFrames = withStkFramesPtr stkFrames $ \c_stkFramesPtr -> newStkFramesFromExisting stkFrames c_emfb_stk_stkframes_clone $ \fun -> fun c_stkFramesPtr

stkFramesAdd :: StkFrames -> StkFrames -> IO StkFrames
stkFramesAdd = stkFramesPureBinaryOp c_emfb_stk_stkframes_add

stkFramesMulHomologs :: StkFrames -> StkFrames -> IO StkFrames
stkFramesMulHomologs = stkFramesPureBinaryOp c_emfb_stk_stkframes_mulHomologs

stkFramesAddInplace :: StkFrames -> StkFrames -> IO ()
stkFramesAddInplace = stkFramesImplaceBinaryOp c_emfb_stk_stkframes_addInplace

stkFramesMulHomologsInplace :: StkFrames -> StkFrames -> IO ()
stkFramesMulHomologsInplace = stkFramesImplaceBinaryOp c_emfb_stk_stkframes_mulHomologsInplace

stkFramesScale :: StkFrames -> Double -> IO StkFrames
stkFramesScale stkFrames scalar = withStkFramesPtr stkFrames $ \c_stkFramesPtr -> newStkFramesFromExisting stkFrames c_emfb_stk_stkframes_scale $ \fun -> fun c_stkFramesPtr ( CDouble scalar )

stkFramesScaleInplace :: StkFrames -> Double -> IO ()
stkFramesScaleInplace stkFrames scalar = withStkFramesPtr stkFrames $ \c_stkFramesPtr -> c_emfb_stk_stkframes_scaleInplace c_stkFramesPtr ( CDouble scalar )
