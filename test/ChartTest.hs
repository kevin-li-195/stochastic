{-#LANGUAGE FlexibleContexts#-}

import Control.Monad.Trans

import Data.Sample
import Data.Sample.Types
import Data.Sample.Lib
import Data.Sample.Chart

import qualified Data.Sequence as S

import System.Random
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

normalInit :: StochProcess
normalInit = normalMC 0 1

skew :: Sampler Double
skew = normal 0 1

skewMC :: StochProcess
skewMC = normalMC 0 1

f :: Double -> StochProcess
f d = do
    sk <- lift skew
    normalMC (d + sk) 1

testChart :: S.Seq (S.Seq Double) -> EC (Layout Integer Double) ()
testChart ss = mcToChart "Test Monte Carlo Chart" ss

normal1 :: StochProcess
normal1 = composeMC 10000 normalInit f

mean :: S.Seq Double -> Double
mean ss = (foldr (+) 0 ss) / fromIntegral (S.length ss)

main = do
    gen <- newStdGen
    let a = runMCN 100 normal1 gen
        fs = fmap (\x -> case S.viewr x of
                         S.EmptyR -> 0
                         xs S.:> val -> val) a
    toFile def "chart.png" $ testChart a
    print $ mean fs
