module Main where

import Data.Stochastic
import Data.Stochastic.Internal
import Data.Stochastic.Types

import qualified Data.Sequence as S

fairCoin :: Sampler Bool
fairCoin = bernoulli 0.5

shadyGambler :: Bool -> Sampler Double
shadyGambler b = if b then discrete [(100, 0.9), (-100, 0.1)]
                 else certain (-100)

gamblerGivenCoin :: Sampler Bool -> Sampler Double
gamblerGivenCoin coin = coin >>= shadyGambler

main = do
    s <- sampleION 1000000 $ gamblerGivenCoin fairCoin
    let wins = S.filter (== 100) s
        losses = S.filter (== (-100)) s
    print $ "Wins with fair coin: " ++ (show $ length wins)
    print $ "Losses with fair coin: " ++ (show $ length losses)
