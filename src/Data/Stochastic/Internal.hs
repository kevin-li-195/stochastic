{-|
 Module         : Data.Stochastic.Internal
 Description    : Internal utility functions for Sample types.
 License        : GPL-3
 Maintainer     : hackage@mail.kevinl.io
 Stability      : experimental

-}
module Data.Stochastic.Internal (
  boxMuller
, closedRnd
, openRnd
, closedOpenRnd
, openClosedRnd
) where

import System.Random

-- | Function to convert values sampled from uniform distribution
-- to a value sampled from a standard normal distribution.
boxMuller :: (Floating a) => a -> a -> a
boxMuller u1 u2 = cos (2 * pi * u2) * (sqrt $ (-2) * (log u1))

-- | Randoms in the interval [0, 1]
closedRnd :: (RandomGen g) => g -> (Double, g)
closedRnd gen = randomR (0, 1.0) gen

-- | Randoms in the interval (0, 1)
openRnd :: (RandomGen g) => g -> (Double, g)
openRnd gen = let (a, g) = closedRnd gen
                   in if a == 0 || a == 1
                      then openRnd g
                      else (a, g)
                      
-- | Randoms in the interval [0, 1)
closedOpenRnd :: (RandomGen g) => g -> (Double, g)
closedOpenRnd gen = let (a, g) = closedRnd gen
                    in if a == 1
                       then closedOpenRnd g
                       else (a, g)
                      
-- | Randoms in the interval (0, 1]
openClosedRnd :: (RandomGen g) => g -> (Double, g)
openClosedRnd gen = let (a, g) = closedRnd gen
                   in if a == 0
                      then openClosedRnd g
                      else (a, g)
