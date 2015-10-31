module FoldVersion where

import Shared
import Control.Monad
import System.IO (stdout)
import Data.ByteString.Builder

exec :: IO ()
exec =
  playNotes
    (SynthState
      [ mkOsc SawWave 1.0
      , mkOsc SawWave (0.5 * 0.999)
      , mkOsc SawWave (2.0 * 1.001) ]
      0.0)
    furEliseNotes

playNotes :: SynthState -> [(Frequency, Double)] -> IO ()
playNotes startSynth notes = do
  let f synth (note, time) = do
        let numSamples = floor (time * 44100)
            samples = replicate numSamples (1 / 44100)
            synth' = setSynthFreq note synth

        foldM (\sn dt -> do
          let sn' = stepSynth dt sn
              sample = sampleSynth sn' * 0.1
          hPutBuilder stdout (int16LE (toPCM sample))
          return sn') synth' samples 
  foldM_ f startSynth notes

data Oscillator
  = Oscillator
  { oscWaveform :: !Waveform
  , oscFreqMod :: !Double
  , oscPhase :: !Phase
  }

data SynthState = SynthState ![Oscillator] !Frequency

mkOsc :: Waveform -> Double -> Oscillator
mkOsc w x = Oscillator w x 0.0

stepOsc :: Frequency -> DeltaTime -> Oscillator -> Oscillator
stepOsc hz dt osc =
  osc { oscPhase = stepPhase (hz * oscFreqMod osc) dt (oscPhase osc) }
  
stepSynth :: DeltaTime -> SynthState -> SynthState
stepSynth dt (SynthState oscs hz) =
  SynthState (map (stepOsc hz dt) oscs) hz

setSynthFreq :: Frequency -> SynthState -> SynthState
setSynthFreq hz (SynthState oscs _) =
  SynthState oscs hz

sampleSynth :: SynthState -> Sample
sampleSynth (SynthState oscs _) =
  sum (map (\(Oscillator wv _ phase) -> sampleWaveform wv phase) oscs)
