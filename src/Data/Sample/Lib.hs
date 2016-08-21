module Data.Sample.Lib where

-- | Function to convert values sampled from uniform distribution
-- to a value sampled from a standard normal distribution.
boxMuller :: (Floating a) => a -> a -> a
boxMuller u1 u2 = cos (2 * pi * u2) * (sqrt $ (-2) * (log u1))

