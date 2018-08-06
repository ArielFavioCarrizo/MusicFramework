{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.Foreign.CTypes(hsctypeconvert) where

import Foreign.C
import Foreign.C.Types

import Data.Word

class HsCTypeBijective a b where
   hsctypeconvert :: a -> b

instance HsCTypeBijective Double CDouble where
   hsctypeconvert haskellValue = CDouble haskellValue

instance HsCTypeBijective Word32 CUInt where
   hsctypeconvert haskellValue = CUInt haskellValue
