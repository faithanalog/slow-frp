module ReactiveVersion where

import Shared
import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Event.Handler
import Control.Applicative
import Control.Monad
import Data.ByteString.Builder
import System.IO (stdout)

exec :: IO ()
exec = do
  -- Create event handlers and functions to fire events
  -- This allows us to control the music network
  (sampleHandler, fireSample) <- newAddHandler
  (freqHandler, fireFreq) <- newAddHandler

  -- Create the actual music event network
  network <- compile $ do
    -- Samples as delta-times
    samples <- fromAddHandler sampleHandler

    -- Frequency for oscillators (note input)
    frequency <- fromChanges 0 freqHandler

    -- Phase of our oscillators as dictated by a changing frequency
    phase <- phaseStepper frequency samples

    let -- Get the oscillator's value at every audio sample point
        osc = oscillator SawWave phase <@ samples

        -- Converts it to a 16 bit value and print it to stdout so we
        -- can redirect it to speakers
        out = (hPutBuilder stdout . int16LE . toPCM . (* 0.1)) <$> osc

    -- Tell the network we actually want to perform the above IO
    reactimate out

  -- Activate the network so it will do things when we fire events
  actuate network

  -- For every note in fur elise, we will have the frequency
  -- and the duration of the note. frequency 0 = rest
  forM_ furEliseNotes $ \(hz, time) -> do

    -- Set the frequency of our oscillator
    fireFreq hz

    -- Number of samples to produce the full note
    let numSamples = floor (time * 44100)

    -- Fire events for each sample to produce audio output
    forM_ (replicate numSamples (1 / 44100)) fireSample


phaseStepper :: (MonadMoment m) => Behavior Frequency -> 
                                   Event DeltaTime ->
                                   m (Behavior Phase)
phaseStepper hz dt =
  accumE 0 (stepPhase <$> hz <@> dt) >>= stepper 0


oscillator :: Waveform -> Behavior Phase -> Behavior Sample
oscillator wv phase = sampleWaveform wv <$> phase
