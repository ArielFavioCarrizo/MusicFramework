{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.Foreign.Objects(createSetterFun) where

import Foreign.C
import Foreign.C.Types

import Data.Word

import Esferixis.Foreign.CTypes

createSetterFun unhandledObjectAction nativeFun = \object value -> unhandledObjectAction object nativeFun (\fun -> fun ( hsctypeconvert value ) )
