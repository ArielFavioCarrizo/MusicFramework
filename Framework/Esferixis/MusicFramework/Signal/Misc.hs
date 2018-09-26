{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Esferixis.MusicFramework.Signal.Misc
   (
     Sectionable(sLength)
   , Section(secSource, secOffset, secLength)
   , sWhole
   , sSubSection
   ) where

import Data.Word
import Data.Maybe

class Sectionable s where
   sLength :: s -> Word64

sWhole :: (Sectionable s) => s -> Section s
sWhole sectionable =
   Section {
        secSource = sectionable
      , secOffset = 0
      , secLength = sLength sectionable
      }

data Section s = Section {
     secSource :: (Sectionable s) => s
   , secOffset :: Word64
   , secLength :: Word64
   }

sSubSection :: (Sectionable s) => Section s -> Word64 -> Word64 -> Section s
sSubSection section offset length =
   let srcOffset = secOffset section
       srcLength = secLength section
   in
      if ( ( offset + length ) < srcLength )
         then
            Section {
                 secSource = secSource section
               , secOffset = srcOffset + offset
               , secLength = length
               }
         else error "Invalid interval"
