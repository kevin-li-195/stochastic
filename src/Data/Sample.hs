module Data.Sample where

import Data.Sample.Types
import qualified Data.Sequence as S

import System.Random

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
sampleN :: (RandomGen g, Sampleable d) => Int -> Sample g d a -> g -> S.Seq a
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
sampleION :: Sampleable d => Int -> Sample StdGen d a -> IO (S.Seq a)
sampleION i s = sampleN i s <$> getStdGen

-- | TODO: Concurrent sampling in the IO monad, with 'RandomGen' splitting.
