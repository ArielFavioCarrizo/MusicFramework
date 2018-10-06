{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFramework.Signal.IO.SharedSections(
     SSTickCmd(
          ssDoIOTick
        , ssDoInplaceTick
        )
   , ShareableSection(
          ShareableSection
        , ssRelativeOffset
        , ssLength
        , ssTickCmd
        )
   , SafeSSList
   , ssEnsureSafety
   , ssPerformTicks
   ) where

import Data.Word
import Data.Maybe
import Data.List
import Control.Monad
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.MusicFramework.Signal.Misc
import Esferixis.MusicFramework.Signal.Operations.Signal

data SharingMode = Immutable | Mutable

data SSTickCmd m =
   SSTickCmd {
        ssDoIOTick :: (Monad m) => m ( m () )
      , ssDoInplaceTick :: (Monad m) => Maybe ( m ( m () ) )
      }

ssTickCmdRemoveInplace :: (Monad m) => SSTickCmd m -> SSTickCmd m
ssTickCmdRemoveInplace tickCmd = tickCmd { ssDoInplaceTick = Nothing }

data ShareableSection m = ShareableSection {
     -- Offset relativo de la sección anterior o del comienzo
     ssRelativeOffset :: Word64
     -- Longitud del offset
   , ssLength :: Word64
     -- Comando de tick
   , ssTickCmd :: (Monad m) => SSTickCmd m
   }

ssRemoveInplaceTick :: (Monad m) => ShareableSection m -> ShareableSection m
ssRemoveInplaceTick section = section { ssTickCmd = ssTickCmdRemoveInplace $ ssTickCmd section }

-- Lista de secciones compartidas segura
data SafeSSList m = SafeSSList ( (Monad m) => [ShareableSection m] )

{-
   Dada una lista de secciones devuelve una lista de secciones segura
-}
ssEnsureSafety :: (Monad m) => [ShareableSection m] -> SafeSSList m
ssEnsureSafety sections = SafeSSList $ ssEnsureSafety_impl sections

ssEnsureSafety_impl :: (Monad m) => [ShareableSection m] -> [ShareableSection m]
ssEnsureSafety_impl [] = []
ssEnsureSafety_impl (section:[]) = [section]
ssEnsureSafety_impl (firstSection:nextSection:remainingSections) =
   let self = ssEnsureSafety_impl
       nextSections = nextSection:remainingSections
   in
      if ( ( ssRelativeOffset nextSection ) >= ( ssLength firstSection ) )
         then firstSection:( self nextSections )
         else (ssRemoveInplaceTick firstSection):(self ((ssRemoveInplaceTick nextSection):remainingSections))

{-
   Dada la lista de secciones segura devuelve una acción que realiza los ticks asincrónicamente.
   Devolviendo una acción que espera por ellos.
-}
ssPerformTicks :: (Monad m) => SafeSSList m -> m ( m() )
ssPerformTicks (SafeSSList []) = return $ return ()
ssPerformTicks (SafeSSList (firstSection:nextSections)) = do
   let tickCmd = ssTickCmd firstSection
   waitTick <-
      case ( ssDoInplaceTick tickCmd ) of
         Just action -> action
         Nothing -> ssDoIOTick tickCmd
   
   waitNextTicks <- ssPerformTicks $ SafeSSList nextSections
   
   return $ waitTick >> waitNextTicks
