{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.Foreign.Objects(setter, exceptionUnsafeSetter) where

import Foreign.C
import Foreign.C.Types

import Data.Word

import Esferixis.Foreign.CTypes

setter unhandledObjectAction nativeFun = \object value -> unhandledObjectAction object nativeFun (\fun -> fun ( hsctypeconvert value ) )
exceptionUnsafeSetter exceptionUnsafeObjectAction nativeFun self value = exceptionUnsafeObjectAction self nativeFun (\fun -> fun value)
