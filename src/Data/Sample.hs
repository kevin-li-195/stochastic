module Data.Sample where

import Control.Monad.Trans
import Control.Monad.Writer

import Data.Sample.Types
import qualified Data.Sequence as S

import System.Random

-- | Function to construct a 'MonteCarlo' computation
-- given an initial computation, a 'MonteCarlo' function,
-- and number of times to apply the function with bind.
composeMC :: Integral i => i -> MonteCarlo -> (Double -> MonteCarlo) -> MonteCarlo
composeMC i mc f = if i <= 0 then mc 
                   else (composeMC (i-1) mc f) >>= f

-- | Sample from the 'MonteCarlo' computation, discarding
-- the new 'RandomGen'.
sampleMC_ :: MonteCarlo -> StdGen -> Double
sampleMC_ ma g = flip sample_ g $ liftM fst $ runWriterT ma

-- | Sample from the 'MonteCarlo' computation, returning
-- the value of type a and a new 'RandomGen'.
sampleMC :: MonteCarlo -> StdGen -> (Double, StdGen)
sampleMC ma g = flip sample g $ liftM fst $ runWriterT ma

-- | Run a 'MonteCarlo' computation and retrieve the recorded
-- results along with a new 'RandomGen'.
runMC :: MonteCarlo -> StdGen -> (S.Seq Double, StdGen)
runMC ma g = flip sample g $ execWriterT ma

-- | Run a 'MonteCarlo' computation and retrieve the recorded
-- results, discarding the new 'RandomGen'.
runMC_ :: MonteCarlo -> StdGen -> S.Seq Double
runMC_ ma g = fst $ runMC ma g

-- | Runs a 'MonteCarlo' computation a given number times
-- and produces a 'Sequence' of 'Sequence's of Doubles.
runMCN :: (Integral i) => i -> MonteCarlo -> StdGen -> S.Seq (S.Seq Double)
runMCN n mc gen = if n <= 0 then S.empty
                  else let (seq, gen') = runMC mc gen
                       in seq S.<| runMCN (n-1) mc gen'

-- | 'MonteCarlo' sample for a normal distribution that records
-- the value sampled from the normal distribution.
normalMC :: Mean -> StDev -> MonteCarlo 
normalMC mean std = do
    sample <- lift $ normal std mean
    tell $ S.singleton sample
    return sample

-- | 'MonteCarlo' sample for a distribution over 'Num's that always
-- returns the same value when sampled, and records that value.
certainMC :: Double -> MonteCarlo 
certainMC a = do
    sample <- lift $ certain a
    tell $ S.singleton sample
    return sample

-- | 'MonteCarlo' sample for a discrete distribution over 'Num's
-- that records the value sampled from the normal distribution.
discreteMC :: [(Double, Double)] -> MonteCarlo 
discreteMC a = do
    sample <- lift $ discrete a
    tell $ S.singleton sample
    return sample

-- | Function to make a 'Sample' out of a provided
-- 'Distribution'.
mkSample :: (RandomGen g, Sampleable d) => d a -> Sample g d a
mkSample d = Sample $ \_ -> d

-- | 'Sample' for a normal distribution with given
-- 'StdGen', 'Mean', and 'StDev'.
normal :: RandomGen g => Mean -> StDev -> Sample g Distribution Double
normal m s = mkSample $ Normal m s

-- | 'Sample' for a Bernoulli distribution with given
-- probability to produce True.
bernoulli :: RandomGen g => Double -> Sample g Distribution Bool
bernoulli f = mkSample $ Bernoulli f

-- | 'Sample' for a discrete distribution with given
-- list of tuples of values of type a and 'Double's 
-- representing the probability of producing each
-- value when sampling from this distribution.
discrete :: RandomGen g => [(a, Double)] -> Sample g Distribution a
discrete [] = error "do not construct empty discrete distributions"
discrete l = mkSample $ Discrete l

-- | 'Sample' for a distribution where we always sample
-- the same value.
certain :: (RandomGen g, Sampleable d) => a -> Sample g d a
certain = mkSample . certainDist

-- | Get one sample of type a from the 'Sample' along with
-- a new 'StdGen'
sample :: (RandomGen g, Sampleable d) => Sample g d a -> g -> (a, g)
sample s g = sampleFrom (runSample s g) g

-- | Get one sample of type a from the 'Sample',
-- discarding the 'RandomGen'
sample_ :: (RandomGen g, Sampleable d) => Sample g d a -> g -> a
sample_ s g = fst $ sample s g

-- | Get a certain number of samples from the 'Sample'
sampleN :: (RandomGen g, Sampleable d, Integral i) => i -> Sample g d a -> g -> S.Seq a
sampleN i s g = if i <= 0 then S.empty
                else let (a, g') = sample s g
                     in a S.<| sampleN (i - 1) s g'

-- | Sample from a 'Sample' of type a using the global
-- random number generator provided by 'getStdGen',
-- returning a new 'StdGen' with the sampled value.
sampleIO :: Sampleable d => Sample StdGen d a -> IO (a, StdGen)
sampleIO s = sample s <$> getStdGen

-- | Sample from a 'Sample' of type a using the global
-- random number generator provided by 'getStdGen',
-- discarding the new 'StdGen'.
sampleIO_ :: Sampleable d => Sample StdGen d a -> IO a
sampleIO_ s = fst <$> (sample s <$> getStdGen)

-- | Produce several samples from the 'Sample' using the random number generator
-- in the IO monad.
sampleION :: (Sampleable d, Integral i) => i -> Sample StdGen d a -> IO (S.Seq a)
sampleION i s = sampleN i s <$> getStdGen

-- | TODO: Concurrent sampling in the IO monad, with 'RandomGen' splitting.
