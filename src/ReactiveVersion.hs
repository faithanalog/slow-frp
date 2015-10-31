module ReactiveVersion where

import Shared
import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Event.Handler
import Control.Applicative
import Control.Monad
import Data.ByteString.Builder
import System.IO (stdout)
import qualified Data.Vector.Unboxed as V
import Data.Monoid

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
    frequency <- fromChanges (fillChunk 0) freqHandler

    -- Phase of our oscillators as dictated by a changing frequency
    phase <- phaseStepper samples frequency

    osc <- do
      let mkOsc detune =
            oscillator SawWave
              <$> phaseStepper
                    samples
                    (V.map (* detune) <$> frequency)
      osc0 <- mkOsc 1.0
      osc1 <- mkOsc (2 * 0.999)
      osc2 <- mkOsc (0.5 * 1.001)
      return (V.zipWith3 (\x y z -> x + y + z) <$> osc0 <*> osc1 <*> osc2)

    lpOsc <- oscillator CosineWave <$> phaseStepper samples (pure (fillChunk (5.3333333 * 2)))
  
    filter <- lowPass samples (V.map ((* 4000) . (+ 1)) <$> lpOsc) osc

    let -- Get the oscillator's value at every audio sample point
        synth = filter <@ samples

        -- Convert chunks of samples to chunks of 16 bit values
        mkBuffer = V.foldl (\x y -> x <> (int16LE . toPCM . (* 0.1) $ y)) mempty

        -- Print samples to stdout so we can hear it
        out = (hPutBuilder stdout . mkBuffer) <$> synth

    -- Tell the network we actually want to perform the above IO
    reactimate out

  -- Activate the network so it will do things when we fire events
  actuate network

  -- For every note in fur elise, we will have the frequency
  -- and the duration of the note. frequency 0 = rest
  forM_ furEliseNotes $ \(hz, time) -> do

    -- Set the frequency of our oscillator
    fireFreq (fillChunk (hz * 0.5))

    -- Number of samples to produce the full note
    let numSamples = floor (time * fromIntegral sampleRate) `div` chunkSize

    -- Fire events for each sample to produce audio output
    forM_ (replicate numSamples (fillChunk timeStep)) fireSample

sampleRate :: Int
sampleRate = 44100

timeStep :: Double
timeStep = 1 / fromIntegral sampleRate

phaseStepper :: (MonadMoment m) => 
                Event a ->
                Behavior (Chunk Frequency) -> 
                m (Behavior (Chunk Phase))
phaseStepper dt hz =
  accumB zeroChunk (stepPhase' <$> hz <@ dt)


stepPhase' :: Chunk Frequency -> Chunk Phase -> Chunk Phase
stepPhase' hz ph =
  V.postscanl'
    (\phase freq -> 
      if freq == 0.0 then
        0.0
      else
        wrapOscPhase (phase + timeStep * freq))
    (V.last ph)
    hz


oscillator :: Waveform -> Behavior (Chunk Phase) -> Behavior (Chunk Sample)
oscillator wv phase = V.map (sampleWaveform wv) <$> phase

lowPass :: (MonadMoment m) =>
           Event a ->
           Behavior (Chunk Frequency) ->
           Behavior (Chunk Sample) ->
           m (Behavior (Chunk Sample))
lowPass t hz samples =
  accumB zeroChunk (step <$> hz <*> samples <@ t)
  where lowPass dt hz x y =
          let rc = 1 / (2 * pi * hz)
              alpha = dt / (rc + dt)
          in alpha * x + (1 - alpha) * y
        step hzs xs ys =
          V.postscanl'
            (\y (x,hz) -> lowPass timeStep hz x y)
            (V.last ys)
            (V.zip xs hzs)
         

type Chunk a = V.Vector Double

chunkSize :: Int
chunkSize = 128

fillChunk :: Double -> Chunk a
fillChunk = V.replicate chunkSize

zeroChunk :: Chunk a
zeroChunk = fillChunk 0

genChunk :: (Int -> Double) -> Chunk a
genChunk f = V.fromList [f x | x <- [0..chunkSize - 1]]


