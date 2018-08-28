{-# LANGUAGE MultiParamTypeClasses #-}

module Esferixis.MusicFramework.BindingsTest.STK.Guitar where

import Data.Word

import Esferixis.MusicFramework.Bindings.STK
import Esferixis.MusicFramework.Bindings.STK.Filewrite
import Esferixis.MusicFramework.Bindings.STK.FileWvOut
import Esferixis.MusicFramework.Bindings.STK.Frames
import Esferixis.MusicFramework.Bindings.STK.Guitar
import Esferixis.MusicFramework.Bindings.STK.Cubic

pitchToFrequency :: Double -> Double
pitchToFrequency pitch = 2.0 ** ( 1.0 / 12.0 * ( pitch - 49.0 ) ) * 440.0 

doSimpleGuitarTest :: IO ()
doSimpleGuitarTest = do
   let nChannels = 1 -- Un canal
   let time = 10 :: Double
   currentSampleRate <- sampleRate
   let nFrames = ceiling ( time * currentSampleRate ) :: Word32

   -- Crear una salida de archivo
   fileWvOut <- newFileWvOut "simpleGuitarTest.wav" nChannels FileWav StkSInt16 1024

   -- Crear frames de entrada y de salida
   inputFrames <- newZeroedStkFrames nFrames nChannels
   outputFrames <- newZeroedStkFrames nFrames nChannels

   -- Crear guitarra
   guitar <- newGuitar 6 ""

   -- Setear su estado inicial
   guitarClear guitar
   
   -- Setear el pluck position para todas las cuerdas
   guitarSetPluckPosition guitar 0.3 (-1)

   -- Tocar notas de prueba
   guitarNoteOn guitar ( pitchToFrequency 30.0 ) 1.0 0
   guitarNoteOn guitar ( pitchToFrequency ( 30.0 + 7.0 ) ) 1.0 1

   -- Setear el loop gain para todas las cuerdas
   guitarSetLoopGain guitar 0.995 (-1)

   -- Sintetizar
   guitarTick guitar inputFrames outputFrames 0 0

   -- Amplificar
   --stkFramesScaleInplace outputFrames 100.0

   -- Crear un filtro cubico
   --cubic <- newCubic
 
   -- Aplicar el filtro
   --cubicTickInplace cubic outputFrames 0

   -- Destruir el filtro
   --deleteCubic cubic

   -- Poner la salida en el archivo
   fileWvOutTick fileWvOut outputFrames

   -- Cerrar el archivo
   closeFileWvOut fileWvOut
