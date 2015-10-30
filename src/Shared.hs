{-# LANGUAGE OverloadedStrings #-}
module Shared where
import Data.String
import Data.Char
import Data.Int (Int16)

type Sample = Double
type Frequency = Double
type Phase = Double
type DeltaTime = Double

data Waveform = SineWave | SquareWave | SawWave | TriangleWave

sampleWaveform :: Waveform -> Phase -> Sample
sampleWaveform wv t
  | t >= 1.0 || t <= -1.0 = sampleWaveform wv (wrapOscPhase t)

sampleWaveform SineWave t =
  sin (pi * 2 * t)

sampleWaveform SquareWave t
  | t <= 0.5 = 1.0
  | otherwise = -1.0

sampleWaveform SawWave t =
  2.0 * (t - fromIntegral (floor (0.5 + t)))

sampleWaveform TriangleWave t =
  2 * abs (sampleWaveform SawWave (t + 0.25)) - 1

wrapOscPhase :: Phase -> Phase
wrapOscPhase x = x - fromIntegral (floor x)

stepPhase :: Frequency -> DeltaTime -> Phase -> Phase
stepPhase hz dt p = wrapOscPhase (p + dt * hz)

toPCM :: Double -> Int16
toPCM x
  | x >  1 = 32767
  | x < -1 = -32767
  | otherwise = round (x * 32767)

-- Duration of an eighth note
noteEighth :: Double
noteEighth = 60 / 160 * 0.5

-- Duration of a quarter note
noteQuarter :: Double
noteQuarter = 60 / 160

furEliseNotes :: [(Double,Double)]
furEliseNotes = map (\(x,y) -> (getFreq x, y))
  [ ("E4" , noteEighth)   -- E4
  , ("D#4", noteEighth)   -- D#
  , ("E4" , noteEighth)   -- E4
  , ("D#4", noteEighth)   -- D#
  , ("E4" , noteEighth)   -- E4
  , ("B3" , noteEighth)   -- B3
  , ("D4" , noteEighth)   -- D4
  , ("C4" , noteEighth)   -- C4
  , ("A3" , noteQuarter)  -- A3
  , ("RST", noteEighth)   -- Rest
  , ("C3",  noteEighth)   -- C3
  , ("E3" , noteEighth)   -- E3
  , ("A3" , noteEighth)   -- A3
  , ("B3" , noteQuarter)  -- B3
  , ("RST", noteEighth)   -- Rest
  , ("E3" , noteEighth)   -- E3
  , ("G#3", noteEighth)   -- G#3
  , ("B3" , noteEighth)   -- B3
  , ("C4" , noteQuarter)  -- C4

  , ("RST", noteEighth)   -- Rest
  , ("E3" , noteEighth)   -- E3
  , ("E4" , noteEighth)   -- E4
  , ("D#4", noteEighth)   -- D#
  , ("E4" , noteEighth)   -- E4
  , ("D#4", noteEighth)   -- D#
  , ("E4" , noteEighth)   -- E4
  , ("B3" , noteEighth)   -- B3
  , ("D4" , noteEighth)   -- D4
  , ("C4" , noteEighth)   -- C4
  , ("A3" , noteQuarter)  -- A3
  , ("RST", noteEighth)   -- Rest
  , ("C3" , noteEighth)   -- C3
  , ("E3" , noteEighth)   -- E3
  , ("A3" , noteEighth)   -- A3
  , ("B3" , noteQuarter)  -- B3
  , ("RST", noteEighth)   -- Rest
  , ("E3" , noteEighth)   -- E3
  , ("C4" , noteEighth)   -- C4
  , ("B3" , noteEighth)   -- B3
  , ("A3" , noteQuarter)  -- A3
  ]

-- Notes stuff
type Note = Int

-- Calculates the frequency of a note, given it's MIDI value
-- A4 = 440hz
-- A4 = Note 69
noteFrequency :: Note -> Frequency
noteFrequency 69 = 440.0
noteFrequency n | n < 0 = noteFrequency 0
                | n > 127 = noteFrequency 127
noteFrequency n = noteFrequency 69 * a ** fromIntegral (n - 69)
  where a = 1.0594630943592953 -- 2^(1/12)

-- Allow frequencies to be written inline as strings
newtype FrequencyS = FrequencyS { getFreq :: Frequency } deriving (Eq, Read, Show)

instance IsString FrequencyS where
  fromString "RST" = FrequencyS 0
  fromString "rst" = FrequencyS 0
  fromString xs = 
    let freq = case xs of
          [note, '#', octave] -> (+ 1) <$> noteNumber note octave
          [note, 'b', octave] -> subtract 1 <$> noteNumber note octave
          [note, octave] -> noteNumber note octave
          _ -> Nothing
    in case freq of
      Nothing -> error ("Invalid note: " ++ xs)
      Just x -> FrequencyS (noteFrequency x)
    where
      noteNumber note octave = (+) <$> noteOffset note <*> octaveBase octave
      octaveBase o
        | not (isDigit o) = Nothing 
        | otherwise = Just ((fromEnum o - fromEnum '0' + 1) * 12)
      noteOffset 'C' = Just 0
      noteOffset 'D' = Just 2
      noteOffset 'E' = Just 4
      noteOffset 'F' = Just 5
      noteOffset 'G' = Just 7
      noteOffset 'A' = Just 9
      noteOffset 'B' = Just 11
      noteOffset n | n >= 'a' && n <= 'f' = noteOffset (toUpper n)
      noteOffset _ = Nothing


