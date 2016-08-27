{-#LANGUAGE GADTs#-}
{-#LANGUAGE RankNTypes#-}
{-#LANGUAGE FlexibleInstances#-}

{-|
 Module         : Data.Stochastic.Types
 Description    : Types used for the stochastic package.
 License        : GPL-3
 Maintainer     : hackage@mail.kevinl.io
 Stability      : experimental

 This module contains the types used
 for the stochastic package.

 WARNING: In its current state, care should be
 taken when using discrete distributions
 as it is never checked that the probabilities
 sum to 1. As is, execution of sampling may fail at run-time
 if probabilities aren't normalized.
-}

module Data.Stochastic.Types (
  Distribution (..)
, Sampleable (..)
, Sample (..)
, StochProcess (..)
, Sampler (..)
, Mean (..)
, StDev (..)
, marsagliaTsang
) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import Data.Stochastic.Internal

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
    DiscreteUniform :: [a] -> Distribution a
    Uniform :: Distribution Double
    Certain :: a -> Distribution a
    Gamma :: Double -> Double -> Distribution Double
    -- ^ Gamma distribution, where the first parameter is the
    -- shape parameter alpha, and the second parameter is the
    -- scale parameter beta.
    Beta :: Double -> Double -> Distribution Double

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
-- to sampleFrom. This lets us obey the monad laws.
instance Sampleable Distribution where
    sampleFrom da g
        = case da of
            Normal mean stdev 
                -> let (a, g')   = closedRnd g 
                       (a', g'') = closedRnd g'
                       s = (stdev * (boxMuller a a')) + mean
                   in (s, g')
            Bernoulli prob    
                -> let (a, g') = closedRnd g
                   in (a <= prob, g')
            Discrete []
                -> error "cannot sample from empty discrete distribution"
            Discrete l        
                -> let (a, g') = closedRnd g
                   in (scan a l, g')
                   where scan lim [] = 
                             if lim <= 0 then error $ "not normalized discrete dist"
                             else error "empty discrete dist"
                         scan lim (x:xs) = 
                             if lim <= snd x then fst x 
                             else scan (lim - snd x) xs
            DiscreteUniform []
                -> error "cannot sample from empty discrete distribution"
            DiscreteUniform l
                -> let (a, g') = closedRnd g
                       prob = 1 / (fromIntegral $ length l)
                   in (l !! (floor $ a / prob), g')
            Uniform
                -> closedRnd g
            Gamma alpha beta
                -> if alpha <= 0 || beta <= 0 then error "alpha and beta parameter cannot be less than or equal to zero in beta distribution"
                   else if alpha > 0 && alpha < 1 then
                        let (a, g') = sampleFrom (Gamma (alpha + 1) beta) g
                            (uni, g'') = openRnd g'
                        in (a * (uni ** (1/alpha)), g'')
                   else let d = alpha - (1/3)
                            c = 1 / sqrt (9 * d)
                            (m, g') =  marsagliaTsang d c g
                        in (m * beta, g')
            Beta alpha beta
                -> let (x, g') = sampleFrom (Gamma alpha 1) g
                       (y, g'') = sampleFrom (Gamma beta 1) g'
                   in (x / (x + y), g'')
            Certain val       
                -> (val, snd $ openRnd g) 
                -- Seemingly unnecessary, but important to obey the monad laws to always produce the same RandomGen each time we sample.
    certainDist = Certain

-- | Marsaglia and Tsang's rejection method
-- for generating Gamma variates with parameters
-- alpha and 1, where 1 is the scale parameter,
-- given d and c.
marsagliaTsang :: (RandomGen g) => Double -> Double -> g -> (Double, g)
marsagliaTsang d c g =
    let (norm, g') = sampleFrom (Normal 0 1) g
        (uni, g'') = sampleFrom Uniform g'
        v = (1 + (c * norm)) ** 3
    in if norm > ((-1)/c) && 
          log uni < ((norm ** 2)/2 + d - (d * v) + (d * log v))
       then (d * v, g'')
       else marsagliaTsang d c g''

-- | Show instance for 'Distribution's.
instance (Show a) => Show (Distribution a) where
    show da = case da of
        Normal mean stdev -> "Normal " ++ show mean ++ " " ++ show stdev
        Bernoulli prob -> "Bernoulli " ++ show prob
        Discrete l -> "Discrete " ++ show l
        DiscreteUniform l -> "DiscreteUniform " ++ show l       
        Certain val -> "Certain " ++ show val

-- | 'Sample' monad containing a random number generator plus a type from which
-- we can sample values of type a
newtype Sample g d a
    = Sample { runSample :: (RandomGen g, Sampleable d) => State g (d a) }

-- | Monad that represents a stochastic process.
-- It allows us to record numeric values as we sample.
type StochProcess
    = WriterT (S.Seq Double) (Sample StdGen Distribution) Double

-- | Monad instance for Sample.
instance (RandomGen g, Sampleable d) => Monad (Sample g d) where
    return x = Sample $ do
                modify (snd . next)
                return $ certainDist x

    (>>=) ma f = Sample $ do
                     modify (snd . next)
                     dist <- runSample ma
                     g <- get
                     let a = fst $ sampleFrom dist g
                     runSample (f a)

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
