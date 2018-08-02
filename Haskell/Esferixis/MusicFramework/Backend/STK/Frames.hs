{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.Backend.STK.Frames
   ( newZeroedStkFrames
   , newValuedStkFrames
   , channelsStkFrames
   , withStkFramesPtr
   , StkFrames
   , StkChannelFrames( stkChannelFrames_frames, stkChannelFrames_nChannel )
   , StkFramesPtr
   , NativeStkFrames ) where

import Foreign.C
import Foreign.Ptr (Ptr, nullPtr, FunPtr)
import Foreign.ForeignPtr

import Data.Word

import Esferixis.MusicFramework.Backend.STK
import Esferixis.MusicFramework.Backend.STK.Internal.Misc

data NativeStkFrames
type StkFramesPtr = Ptr NativeStkFrames

foreign import ccall "emfb_stk_stkframes_new_zero" c_emfb_stk_frames_new_zero :: Ptr CString -> CUInt -> CUInt -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_new_valued" c_emfb_stk_frames_new_valued :: Ptr CString -> CFloat -> CUInt -> CUInt -> IO StkFramesPtr
foreign import ccall "emfb_stk_stkframes_channels" c_emfb_stk_frames_channels :: StkFramesPtr -> IO CUInt
foreign import ccall "&emfb_stk_stkframes_delete" c_emfb_stk_frames_delete_ptr :: FunPtr ( Ptr NativeStkFrames -> IO () )

data StkFrames = StkFrames ( ForeignPtr NativeStkFrames )
framesForeignPtr (StkFrames a) = a

data StkChannelFrames = StkChannelFrames { stkChannelFrames_frames :: StkFrames
                                         , stkChannelFrames_nChannel :: Word32
                                         }

newZeroedStkFrames :: Word32 -> Word32 -> IO StkFrames
newZeroedStkFrames nFrames nChannels = do
   frames_rawptr <- handleStkExcept ( \c_exceptDescPtr -> c_emfb_stk_frames_new_zero c_exceptDescPtr ( CUInt nFrames ) ( CUInt nChannels ) )
   frames_foreignptr <- ( newForeignPtr c_emfb_stk_frames_delete_ptr frames_rawptr )
   return ( StkFrames frames_foreignptr )

newValuedStkFrames :: Float -> Word32 -> Word32 -> IO StkFrames
newValuedStkFrames value nFrames nChannels = do
   frames_rawptr <- handleStkExcept ( \c_exceptDescPtr -> c_emfb_stk_frames_new_valued c_exceptDescPtr (CFloat value) ( CUInt nFrames ) ( CUInt nChannels ) )
   frames_foreignptr <- ( newForeignPtr c_emfb_stk_frames_delete_ptr frames_rawptr )
   return ( StkFrames frames_foreignptr )

channelsStkFrames :: StkFrames -> IO Word32
channelsStkFrames frames = do
   channels <- withForeignPtr ( framesForeignPtr frames ) (\frames_ptr -> c_emfb_stk_frames_channels frames_ptr )
   return ( fromIntegral channels )

withStkFramesPtr :: StkFrames -> ( Ptr NativeStkFrames -> IO a ) -> IO a
withStkFramesPtr frames fun = withForeignPtr ( framesForeignPtr frames ) fun
