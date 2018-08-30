{-# LANGUAGE Rank2Types #-}

module Esferixis.MusicFramework.Backend.Common.Signal.IOSignal
   () where

import Esferixis.MusicFramework.Signal

class MutableSignalChunk msc where
   mscNew :: Word32 -> Word32 -> IO msc
   mscLength :: msc -> Word32
   mscCopy :: sc -> Word32 -> Word32 -> IO ()
   mscDelete :: IO ()
