{-|
 Module         : Data.Sample
 Description    : Types used for Sample.
 License        : GPL-3
 Maintainer     : hackage@mail.kevinl.io
 Stability      : experimental

 This module contains convenience functions to
 construct 'Sample's or 'StochProcess'es corresponding
 to several probability distributions.

 It also contains functions that can be used for
 running the constructed 'StochProcess'es and generating
 datapoints, or sampling from a constructed 'Sample'.

 Some examples for usage can be found here: <http://kevinl.io/posts/2016-08-17-sampling-monad.html>
-}

module Data.Sample where

import Control.Monad.Trans
import Control.Monad.Writer

import Data.Sample.Types
import qualified Data.Sequence as S

import System.Random

-- | Function to construct a 'StochProcess' computation
-- given an initial computation, a 'StochProcess' function,
-- and number of times to apply the function with bind.
composeProcess :: Integral i => i -> StochProcess -> (Double -> StochProcess) -> StochProcess
composeProcess i pr f = if i <= 0 then pr 
                   else (composeProcess (i-1) pr f) >>= f

-- | Sample from the 'StochProcess' computation, discarding
-- the new 'RandomGen'.
sampleProcess_ :: StochProcess -> StdGen -> Double
sampleProcess_ ma g = flip sample_ g $ liftM fst $ runWriterT ma

-- | Sample from the 'StochProcess' computation, returning
-- the value of type a and a new 'RandomGen'.
sampleProcess :: StochProcess -> StdGen -> (Double, StdGen)
sampleProcess ma g = flip sample g $ liftM fst $ runWriterT ma

-- | Get a certain number of samples from the 'StochProcess' computation.
sampleProcessN :: (Integral i) => i -> StochProcess -> StdGen -> S.Seq Double
sampleProcessN i ma g = if i <= 0 then S.empty
                   else let (a, gen) = sampleProcess ma g
                   in a S.<| sampleProcessN (i-1) ma gen

-- | Run a 'StochProcess' computation and retrieve the recorded
-- results along with a new 'RandomGen'.
runProcess :: StochProcess -> StdGen -> (S.Seq Double, StdGen)
runProcess ma g = flip sample g $ execWriterT ma

-- | Run a 'StochProcess' computation and retrieve the recorded
-- results, discarding the new 'RandomGen'.
runProcess_ :: StochProcess -> StdGen -> S.Seq Double
runProcess_ ma g = fst $ runProcess ma g

-- | Runs a 'StochProcess' computation a given number times
-- and produces a 'Sequence' of 'Sequence's of Doubles.
-- | Get a certain number of samples from the 'Sample'
runProcessN :: (Integral i) => i -> StochProcess -> StdGen -> S.Seq (S.Seq Double)
runProcessN n pr gen = if n <= 0 then S.empty
                  else let (seq, gen') = runProcess pr gen
                       in seq S.<| runProcessN (n-1) pr gen'

-- | 'StochProcess' sample for a normal distribution that records
-- the value sampled from the normal distribution.
normalProcess :: Mean -> StDev -> StochProcess 
normalProcess mean std = do
    sample <- lift $ normal mean std
    tell $ S.singleton sample
    return sample

-- | 'StochProcess' sample for a distribution over 'Double's that always
-- returns the same value when sampled, and records that value.
certainProcess :: Double -> StochProcess 
certainProcess a = do
    sample <- lift $ certain a
    tell $ S.singleton sample
    return sample

-- | 'StochProcess' sample for a discrete distribution over 'Double's
-- that records the value sampled from the normal distribution.
discreteProcess :: [(Double, Double)] -> StochProcess 
discreteProcess a = do
    sample <- lift $ discrete a
    tell $ S.singleton sample
    return sample

-- | 'StochProcess' sample for a uniform distribution over 'Double's
-- that records the value sampled from it.
uniformProcess :: [Double] -> StochProcess
uniformProcess l = do
    sample <- lift $ uniform l
    tell $ S.singleton sample
    return sample

-- | Function to make a 'Sample' out of a provided
-- 'Distribution'.
mkSample :: (RandomGen g, Sampleable d) => d a -> Sample g d a
mkSample d = Sample $ \g -> (d, snd $ next g)

-- | 'Sample' for a normal distribution with given
-- 'StdGen', 'Mean', and 'StDev'.
normal :: RandomGen g => Mean -> StDev -> Sample g Distribution Double
normal m s = mkSample $ Normal m $ abs s

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

-- | 'Sample' for a uniform distribution
-- given a list of provided values.
uniform :: (RandomGen g) => [a] -> Sample g Distribution a
uniform l = mkSample $ Uniform l

-- | 'Sample' for a distribution where we always sample
-- the same value.
certain :: (RandomGen g, Sampleable d) => a -> Sample g d a
certain = mkSample . certainDist

-- | Get one sample of type a from the 'Sample' along with
-- a new 'StdGen'.
--
-- We do an extra 'next' in order to get one more
-- 'RandomGen' because when we sample from normal
-- distributions, we consume one extra 'RandomGen'.
sample :: (RandomGen g, Sampleable d) => Sample g d a -> g -> (a, g)
sample s g = let (dist, g') = runSample s g
                 (a, g'') = sampleFrom dist g'
             in (a, snd $ next g'')

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
-- random number generator provided by 'newStdGen',
-- returning a new 'StdGen' with the sampled value.
sampleIO :: Sampleable d => Sample StdGen d a -> IO (a, StdGen)
sampleIO s = sample s <$> newStdGen

-- | Sample from a 'Sample' of type a using the global
-- random number generator provided by 'newStdGen',
-- discarding the new 'StdGen'.
sampleIO_ :: Sampleable d => Sample StdGen d a -> IO a
sampleIO_ s = fst <$> (sample s <$> newStdGen)

-- | Produce several samples from the 'Sample' using the random number generator
-- in the IO monad.
sampleION :: (Sampleable d, Integral i) => i -> Sample StdGen d a -> IO (S.Seq a)
sampleION i s = sampleN i s <$> newStdGen

-- | TODO: Concurrent sampling in the IO monad, with 'RandomGen' splitting.
