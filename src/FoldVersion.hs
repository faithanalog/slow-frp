module FoldVersion where

import Shared
import Control.Monad
import System.IO (stdout)
import Data.ByteString.Builder

exec :: IO ()
exec = do

  let f synth (hz, time) = do
        let numSamples = floor (time * 44100)
            samples = replicate numSamples (1 / 44100)
            synth' = setSynthFreq hz synth

        foldM (\sn dt -> do
          let sn' = stepSynth dt sn
              sample = sampleSynth sn' * 0.1
          hPutBuilder stdout (int16LE (toPCM sample))
          return sn') synth' samples


      mkOsc x = Oscillator SawWave x 0.0


      startSynth = SynthState
                    [ mkOsc 1.0 ]
                    0.0


  foldM_ f startSynth furEliseNotes

data Oscillator
  = Oscillator
  { oscWaveform :: !Waveform
  , oscFreqMod :: !Double
  , oscPhase :: !Phase
  }

data SynthState = SynthState ![Oscillator] !Frequency

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
