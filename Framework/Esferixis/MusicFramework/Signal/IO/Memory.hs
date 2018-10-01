{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFramework.Signal.IO.Memory(
     ShareableSection(
          ShareableSection
        , ssOffset
        , ssLength
        , ssDoAction
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
     ssRelativeOffset :: Word64 -- ATENCIÓN: Offset relativo a sección anterior
   , ssLength :: Word64 -- Longitud del offset
   , ssDoAction :: (Monad m) => SharingMode -> m ( m () ) -- Acción que realiza la operación asincrónicamente y devuelve una acción que espera por ella
   }

{-
   Dada una lista de secciones devuelve una acción
   que realiza las acciones de cada sección asincrónicamente
   y devuelve otra acción que espera por ellas
-}
performWithSharedSectionable :: (Monad m) => [ShareableSection m] -> m ( m () )
performWithSharedSectionable sections = return $ return () -- FIXME: Implementar
