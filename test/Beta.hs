import Control.Monad.Trans

import Data.Foldable
import Data.Stochastic
import Data.Stochastic.Types
import Data.Stochastic.Internal
import Data.Stochastic.Chart

import qualified Data.Sequence as S

import System.Random
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

beta1 :: Sampler Double
beta1 = beta 0.5 0.5

beta2 :: Sampler Double
beta2 = beta 0.5 2

gamma1 :: Sampler Double
gamma1 = gamma 3 2

gamma2 :: Sampler Double
gamma2 = gamma 3 0.5

main = do
    gen <- newStdGen
    let betas1 = sampleN 100000 beta1 gen
    let betas2 = sampleN 100000 beta2 gen
    let gammas1 = sampleN 100000 gamma1 gen
    let gammas2 = sampleN 100000 gamma2 gen
    renderableToFile def "charts/beta1.png" $ toRenderable $ histogram "Beta1" $ toList betas1
    renderableToFile def "charts/beta2.png" $ toRenderable $ histogram "Beta2" $ toList betas2
    renderableToFile def "charts/gamma1.png" $ toRenderable $ histogram "Gamma1" $ toList gammas1
    renderableToFile def "charts/gamma2.png" $ toRenderable $ histogram "Gamma2" $ toList gammas2
