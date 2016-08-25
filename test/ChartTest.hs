{-#LANGUAGE FlexibleContexts#-}

import Control.Monad.Trans

import Data.Stochastic
import Data.Stochastic.Types
import Data.Stochastic.Internal
import Data.Stochastic.Chart

import qualified Data.Sequence as S

import System.Random
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

normalInit :: StochProcess
normalInit = normalProcess 0 1

skew :: Sampler Double
skew = normal 0 1

skewProcess :: StochProcess
skewProcess = normalProcess 0 1

f :: Double -> StochProcess
f d = do
    sk <- lift skew
    normalProcess (d + sk) 1

testChart :: S.Seq (S.Seq Double) -> EC (Layout Integer Double) ()
testChart ss = processToChart "Test Monte Carlo Chart" ss

normal1 :: StochProcess
normal1 = composeProcess 10000 normalInit f

mean :: S.Seq Double -> Double
mean ss = (foldr (+) 0 ss) / fromIntegral (S.length ss)

main = do
    gen <- newStdGen
    let a = runProcessN 100 normal1 gen
        fs = fmap (\x -> case S.viewr x of
                         S.EmptyR -> 0
                         xs S.:> val -> val) a
    toFile def "chart.png" $ testChart a
    print $ mean fs
