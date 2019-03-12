-- |
-- Module      :  Esferixis.MusicFramework.HMidiBackend
-- Copyright   :  (c) 2019 Ariel Favio Carrizo
-- License     :  BSD-3-Clause
-- Stability   : experimental
-- Portability : ghc

module Esferixis.MusicFramework.HMidiBackend(
   play,
   openByDeviceName
   ) where

import Esferixis.MusicFramework.MIDI
import qualified System.MIDI as HM
import qualified System.MIDI as HM.Base
import Control.Concurrent
import Control.Monad

toHMidiMsg :: MidiMsg -> HM.MidiMessage'
toHMidiMsg (NoteOff key velocity) = HM.NoteOff key velocity
toHMidiMsg (NoteOn key velocity) = HM.NoteOn key velocity
toHMidiMsg (PolyAftertouch key pressure) = HM.PolyAftertouch key pressure
toHMidiMsg (CC cId value) = HM.CC cId value
toHMidiMsg (Aftertouch value) = HM.Aftertouch value
toHMidiMsg (PitchWheel value) = HM.PitchWheel value

play :: HM.Connection -> [MidiCmd] -> IO ()
play connection ( (ChannelMsgCmd (ChannelMsg nChannel midiMsg) ) :nextCmd ) = do
   HM.send connection $ HM.MidiMessage nChannel $ toHMidiMsg midiMsg
   play connection nextCmd
play connection ( (TimestampDelta timeDelta) :nextCmd ) = do
   threadDelay timeDelta
   play connection nextCmd
play connection [] =
   return ()

openByDeviceName deviceName = do
   destinations <- HM.enumerateDestinations
   filteredDestinations <- filterM
      (\eachDestination -> do
          eachDeviceName <- HM.getName eachDestination
          return $ eachDeviceName == deviceName )
      destinations

   HM.openDestination $ head filteredDestinations
