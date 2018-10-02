{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFramework.Signal.IO.Memory(
     ShareableSection(
          ShareableSection
        , ssRelativeOffset
        , ssLength
        , ssSplit
        , ssDoAction
        , ssNextSection
        )
   ) where

import Data.Word
import Data.Maybe
import Control.Monad
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.MusicFramework.Signal.Misc
import Esferixis.MusicFramework.Signal.Operations.Signal

data SharingMode = Immutable | Mutable

data ShareableSection m = ShareableSection {
     -- Offset relativo a sección anterior
     ssRelativeOffset :: Word64
     -- Longitud del offset
   , ssLength :: Word64
     -- Parte la sección en el offset especificado. Devuelve la primer parte de la sección, la segunda parte, y las demás secciones vienen después de ésta.
   , ssSplit :: Word64 -> ShareableSection m
     -- Dado en modo de compartición devuelve una acción que realiza la operación asociada asincrónicamente y devuelve una acción que espera por ella
   , ssDoAction :: (Monad m) => SharingMode -> m ( m () )
     -- Devuelve la sección con el offset siguiente
   , ssNextSection :: Maybe (ShareableSection m)
   }

{-
   Dada la primer sección devuelve una acción
   que realiza las acciones de la sección y
   sus siguientes asincrónicamente,
   y devuelve otra acción que espera por ellas
-}
performWithSharedSectionable :: (Monad m) => ShareableSection m -> m ( m () )
performWithSharedSectionable firstSection = return $ return ()
