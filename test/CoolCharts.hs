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

drift :: Sampler Double
drift = normal 0 1

f :: Double -> StochProcess
f samp = do
    dr <- lift drift
    normalProcess (dr + samp) 1

testChart :: S.Seq (S.Seq Double) -> EC (Layout Integer Double) ()
testChart ss = processToChart "Random Walk" ss

normal1 :: StochProcess
normal1 = composeProcess 10000 normalInit f

main = do
    gen <- newStdGen
    let a = runProcessN 10 normal1 gen
    toFile def "chart.png" $ testChart $ a
