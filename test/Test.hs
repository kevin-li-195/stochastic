module Main where

import Data.Sample
import Data.Sample.Lib
import Data.Sample.Types

import qualified Data.Sequence as S

fairCoin :: Sampler Bool
fairCoin = bernoulli 0.5

headsCoin :: Sampler Bool
headsCoin = bernoulli 0.9

shadyGambler :: Bool -> Sampler Double
shadyGambler b = if b then discrete [(100, 0.8), (-100, 0.2)]
                 else certain (-100)

gamblerGivenCoin :: Sampler Bool -> Sampler Double
gamblerGivenCoin coin = coin >>= shadyGambler

main = do
    c <- sampleION 1000 $ fairCoin
    let c' = S.filter id c
        c'' = S.filter (not . id) c
    print "Coin:"
    print $ "heads: " ++ (show $ length c')
    print $ "tails: " ++ (show $ length c'')

    s <- sampleION 1000 $ gamblerGivenCoin fairCoin
    let s' = S.filter (== 100) s
        s'' = S.filter (== (-100)) s
    print "Wins with fair coin: "
    print $ length s'
    print "Losses with fair coin: "
    print $ length s''

    h <- sampleION 1000 $ headsCoin
    let h' = S.filter id h
        h'' = S.filter (not . id) h
    print "Coin:"
    print $ "heads: " ++ (show $ length h')
    print $ "tails: " ++ (show $ length h'')

    t <- sampleION 100000 $ gamblerGivenCoin headsCoin
    let t' = S.filter (== 100) t
        t'' = S.filter (== (-100)) t
    print "Wins with heads coin: "
    print $ length t'
    print "Losses with heads coin: "
    print $ length t''
