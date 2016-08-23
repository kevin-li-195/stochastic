module Data.Sample.Lib where

import System.Random

import Numeric.MathFunctions.Constants

-- | Function to convert values sampled from uniform distribution
-- to a value sampled from a standard normal distribution.
boxMuller :: (Floating a) => a -> a -> a
boxMuller u1 u2 = cos (2 * pi * u2) * (sqrt $ (-2) * (log u1))

-- | Randoms that aren't too small or equal to 1.
decentRandom :: (RandomGen g) => g -> (Double, g)
decentRandom gen = let (sampled, newG) = decentRandom gen
                   in if sampled <= m_epsilon || sampled == 1
                      then decentRandom newG
                      else (sampled, newG)
