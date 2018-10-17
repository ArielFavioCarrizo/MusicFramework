{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFramework.Signal.IO.SChunk(
     SChunk(scLength)
   , STupleChunk
   ) where

import Data.Word
import Data.Maybe
import Data.Functor
import Esferixis.Control.Concurrency.AsyncIO
import Esferixis.Control.Concurrency.Promise
import Esferixis.Control.Concurrency.SharedRef
import Esferixis.MusicFramework.Signal.Misc
import Esferixis.MusicFramework.Signal.Operations.Signal

{-
   Representación de chunk de señal
-}
class SChunk ch where
   scLength :: ch -> Word64 -- Longitud del chunk de señal

-- Chunk de tupla de señales
data STupleChunk lch rch = STupleChunk {
     stcLeft :: (SChunk lch) => lch
   , stcRight :: (SChunk rch) => rch
   }

instance (SChunk lch, SChunk rch) => SChunk (STupleChunk lch rch) where
   scLength stc = scLength $ stcLeft stc

mkSTupleChunk :: (SChunk lch, SChunk rch) => lch -> rch -> STupleChunk lch rch
mkSTupleChunk leftChunk rightChunk =
   if ( ( scLength leftChunk ) == ( scLength rightChunk ) )
      then
         STupleChunk {
              stcLeft = leftChunk
            , stcRight = rightChunk
            }
      else
         error "Chunk size mismatch"
