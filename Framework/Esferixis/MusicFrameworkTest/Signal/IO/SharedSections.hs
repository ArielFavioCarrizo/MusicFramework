{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Esferixis.MusicFrameworkTest.Signal.IO.SharedSections(
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
import Esferixis.MusicFramework.Signal.IO.SharedSections

mkSSTickCmd :: String -> SSTickCmd IO
mkSSTickCmd label =
   SSTickCmd {
        ssDoIOTick = do
           putStrLn $ "Run I/O " ++ label
           return $ putStrLn $ "Wait I/O " ++ label
      , ssDoInplaceTick = Just $ do
           putStrLn $ "Run inplace " ++ label
           return $ putStrLn $ "Wait inplace I/O " ++ label
      }

mkShareableSection :: String -> Word64 -> Word64 -> ShareableSection IO
mkShareableSection label relativeOffset length =
   ShareableSection {
        ssRelativeOffset = relativeOffset
      , ssLength = length
      , ssTickCmd = mkSSTickCmd $ "(" ++ label ++ ", offset: " ++ (show relativeOffset) ++ ", length: " ++ (show length)
      }
