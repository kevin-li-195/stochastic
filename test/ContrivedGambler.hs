import Data.Foldable

import Data.Sample
import Data.Sample.Lib
import Data.Sample.Types
import Data.Sample.Chart

import qualified Data.Sequence as S

import System.Random

import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

contrivedGambler :: Bool -> Sampler Double
contrivedGambler b = if b
                     then normal 100 20
                     else normal (-100) 10

fairCoin :: Sampler Bool
fairCoin = bernoulli 0.5

gamblerWithCoin :: Sampler Double
gamblerWithCoin = fairCoin >>= contrivedGambler

main = do
    gen <- newStdGen
    let winnings = sampleN 100000 gamblerWithCoin gen
    renderableToFile def "contrived.png" $ toRenderable $ histogram "Contrived Gambler" $ toList winnings
