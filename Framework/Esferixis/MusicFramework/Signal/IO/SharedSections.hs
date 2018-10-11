{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFramework.Signal.IO.SharedSections(
     ShareableSection(
          ShareableSection
        , ssRelativeOffset
        , ssLength
        , ssContext
        )
   , SharingType(Isolated, Shared)
   , VerifiedSS
   , ssDisplaceLeft
   , ssMerge
   , ssVerifiedSSUnwrap
   , ssSharing
   ) where

import Data.Word
import Data.Maybe
import Data.List
import Data.Tuple

data ShareableSection c = ShareableSection {
     -- Offset relativo de la sección anterior o del comienzo
     ssRelativeOffset :: Word64
     -- Longitud del offset
   , ssLength :: Word64
     -- Contexto
   , ssContext :: c
   }

-- Tipo de compartido
data SharingType = Isolated | Shared

-- Lista de secciones compartidas verificada
data VerifiedSS c = VerifiedSS [ ShareableSection (c, SharingType) ]

{-
   Dada una sección la translada hacia la izquierda con el desplazamiento
   especificado
-}
ssDisplaceLeft :: ShareableSection c -> Word64 -> ShareableSection c
ssDisplaceLeft section delta =
   let srcOffset = ( ssRelativeOffset section ) 
   in
      if ( delta <= srcOffset )
         then section { ssRelativeOffset = srcOffset - delta }
         else error "Delta must be less or equal than offset"

{-
   Dada dos listas de secciones compartidas genera
   una sola lista con secciones de ambas de listas
-}
ssMerge :: [ShareableSection c] -> [ShareableSection c] -> [ShareableSection c]
ssMerge [] [] = []
ssMerge [section] [] = [section]
ssMerge [] [section] = [section]
ssMerge (firstSection1:nextSections1) (firstSection2:nextSections2) =
   let offset1 = ssRelativeOffset firstSection1
       offset2 = ssRelativeOffset firstSection2
   in
      if ( offset1 <= offset2 )
         then firstSection1:(ssMerge nextSections1 $ (ssDisplaceLeft firstSection2 offset1):nextSections2 )
         else ssMerge (firstSection2:nextSections2) (firstSection1:nextSections1)

{-
   Dado una sección compartible devuelve la misma
   con el valor adosado especificado
-}
ssContextAppend :: ShareableSection c -> a -> ShareableSection (c, a)
ssContextAppend section value = section { ssContext = (ssContext section, value) }

{-
   Dado una sección compartible con un adosado de compartido
   devuelve la misma como compartid
-}
ssSetShared :: ShareableSection (c, SharingType) -> ShareableSection (c, SharingType)
ssSetShared section = section { ssContext = ( fst (ssContext section), Shared ) }

{-
   Dado una lista certificada de secciones compartidas
   verificadas la desenvuelve
-}
ssVerifiedSSUnwrap :: VerifiedSS c -> [ShareableSection (c, SharingType)]
ssVerifiedSSUnwrap ( VerifiedSS sections ) = sections

{-
   Dada una lista de secciones compartibles devuelve
   una lista certificada de las secciones con el contexto y el tipo
   de compartido especificado
-}
ssSharing :: [ShareableSection c] -> VerifiedSS c
ssSharing [] = VerifiedSS $ []
ssSharing (firstSection:nextSections) = VerifiedSS $ ssSharingWithEvaluated ( ssContextAppend firstSection Isolated ) nextSections

ssSharingWithEvaluated :: ShareableSection (c, SharingType) -> [ShareableSection c] -> [ShareableSection (c, SharingType)]
ssSharingWithEvaluated firstSection [] = [firstSection]
ssSharingWithEvaluated firstSection (nextSection:remainingSections) =
   let nextSections = nextSection:remainingSections
   in
      if ( ( ssRelativeOffset nextSection ) >= ( ssLength firstSection ) )
         then firstSection:(ssVerifiedSSUnwrap $ ssSharing nextSections)
         else (ssSetShared firstSection):(ssSharingWithEvaluated (ssContextAppend nextSection Shared) remainingSections)
