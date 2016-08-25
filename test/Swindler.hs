import Data.Stochastic
import Data.Stochastic.Internal
import Data.Stochastic.Types

import qualified Data.Sequence as S

import System.Random

swindler :: Int -> Bool
swindler i = i == 1

die :: Sampler Int
die = uniform [1, 2, 3, 4, 5, 6]

swindlersDice :: Sampler Bool
swindlersDice = do
    s1 <- die
    s2 <- die
    s3 <- die
    return (swindler s1 || swindler s2 || swindler s3)

main = do
    gen <- newStdGen
    let a = sampleN 100000 swindlersDice gen
    print $ length $ S.filter id a
    print $ length $ S.filter (not . id) a
