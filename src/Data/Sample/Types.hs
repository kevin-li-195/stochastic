{-#LANGUAGE GADTs#-}
{-#LANGUAGE RankNTypes#-}
{-#LANGUAGE FlexibleInstances#-}

{-|
 Module         : Data.Sample.Types
 Description    : Types used for Sample.
 License        : GPL-3
 Maintainer     : hackage@mail.kevinl.io
 Stability      : experimental

 This module contains the types
 for the stat-sampling package.

 WARNING: In its current state, care should be
 taken when using discrete distributions
 as it is never checked that the probabilities
 sum to 1. As is, execution of sampling may fail at run-time
 if probabilities aren't normalized.
-}

module Data.Sample.Types where

import Control.Monad
import Control.Monad.Writer

import Data.Sample.Lib

import qualified Data.Sequence as S

import System.Random

-- | Datatype representing parameterized probability distributions
-- over values of type a. GADTs are used to restrict types
-- of certain distributions (e.g. normal distributions can
-- only be defined over floating point numbers)
data Distribution a where
    Normal :: Mean -> StDev -> Distribution Double
    Bernoulli :: Double -> Distribution Bool
    Discrete :: [(a, Double)] -> Distribution a
    Certain :: a -> Distribution a

-- | Class of types from which samples can be obtained.
class Sampleable d where
    -- | Constructor for a datatype from which we always
    -- sample the same value.
    certainDist :: a -> d a
    -- | Sample from the sampleable datatype using a 'RandomGen'
    -- returning a new 'RandomGen'.
    sampleFrom :: (RandomGen g) => d a -> g -> (a, g)

-- | 'Sampleable' instance for 'Distribution'. We ensure
-- that we always pass the *next* 'RandomGen' provided
-- to sampleFrom. This allows us to obey the monad laws.
instance Sampleable Distribution where
    sampleFrom da g
        = case da of
            Normal mean stdev 
                -> let (a, g')   = decentRandom g 
                       (a', g'') = decentRandom g'
                       s = (stdev * (boxMuller a a')) + mean
                   in (s, g')
            Bernoulli prob    
                -> let (a, g') = decentRandom g
                   in (a <= prob, g')
            Discrete l        
                -> let (a, g') = decentRandom g
                   in (scan a l, g')
                   where scan lim [] = 
                             if lim <= 0 then error $ "not normalized discrete dist"
                             else error "empty discrete dist"
                         scan lim (x:xs) = 
                             if lim <= snd x then fst x 
                             else scan (lim - snd x) xs
            Certain val       
                -> (val, snd $ decentRandom g) 
                -- Seemingly unnecessary, but important to obey the monad laws to always produce the same RandomGen each time we sample.
    certainDist = Certain

-- | Show instance for 'Distribution's.
instance (Show a) => Show (Distribution a) where
    show da = case da of
        Normal mean stdev -> "Normal dist: Mean = " ++ show mean ++ " StDev = " ++ show stdev
        Bernoulli prob -> "Bernoulli dist: Prob = " ++ show prob
        Discrete l -> "Discrete dist: List is = " ++ show l
        Certain val -> "Certain dist: val is: " ++ show val

-- | 'Sample' monad containing a random number generator plus a type from which
-- we can sample values of type a
newtype Sample g d a
    = Sample { runSample :: (RandomGen g, Sampleable d) => g -> (d a, g) }

-- | Monad that represents a stochastic process.
-- It allows us to record numeric values as we sample.
type StochProcess
    = WriterT (S.Seq Double) (Sample StdGen Distribution) Double

-- | Monad instance for Sample.
instance (RandomGen g, Sampleable d) => Monad (Sample g d) where
    return x = Sample $ \g -> (certainDist x, snd $ next g)
    (>>=) ma f = let func = runSample ma
                 in Sample $ \g -> 
                       let (dist, g') = func g
                           (a, g'') = sampleFrom dist g'
                       in runSample (f a) g''

-- | Trivial 'Functor' instance for 'Sample' 'StdGen' 'Distribution'.
instance (RandomGen g, Sampleable s) => Functor (Sample g s) where
    fmap = liftM

-- | Trivial 'Applicative' instance for 'Sample' 'StdGen' 'Distribution'.
instance (RandomGen g, Sampleable s) => Applicative (Sample g s) where
    pure = return
    (<*>) = ap

-- | Type synonym for shorter type annotations for 'Sample'.
type Sampler a = Sample StdGen Distribution a

-- | Type synonym for 'Double' so that the 
-- type annotation for the 'Normal' constructor is more informative.
type Mean = Double

-- | Type synonyms for 'Double' so that the 
-- type annotation for the 'Normal' constructor is more informative.
type StDev = Double
