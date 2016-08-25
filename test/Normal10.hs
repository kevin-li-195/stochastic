import Data.Stochastic
import Data.Stochastic.Types
import Data.Stochastic.Internal

import System.Random

normalInit :: StochProcess
normalInit = normalProcess 10 3

f :: Double -> StochProcess
f d = if d < 7 then normalProcess (0.9 * d) (0.3 * d)
  else if d > 13 then normalProcess (1.1 * d) (0.3 * d)
  else normalProcess d (0.3 * d)

normal10 :: StochProcess
normal10 = composeProcess 10 normalInit f

main = do
    gen <- newStdGen
    print "normal10"
    print $ runProcessN 100000 normal10 gen
