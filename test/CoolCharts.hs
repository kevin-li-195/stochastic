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

drift :: Sampler Double
drift = normal 0 1

f :: Double -> StochProcess
f samp = do
    dr <- lift drift
    normalMC (dr + samp) 1

testChart :: S.Seq (S.Seq Double) -> EC (Layout Integer Double) ()
testChart ss = mcToChart "Random Walk" ss

normal1 :: StochProcess
normal1 = composeMC 10000 normalInit f

main = do
    gen <- newStdGen
    let a = runMC_ normal1 gen
    toFile def "chart.png" $ testChart $ S.singleton a
