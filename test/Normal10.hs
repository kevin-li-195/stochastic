import Data.Sample
import Data.Sample.Types
import Data.Sample.Lib

import System.Random

normalInit :: MonteCarlo
normalInit = normalMC 10 3

f :: Double -> MonteCarlo
f d = if d < 7 then normalMC (0.9 * d) (0.3 * d)
  else if d > 13 then normalMC (1.1 * d) (0.3 * d)
  else normalMC d (0.3 * d)

normal10 :: MonteCarlo
normal10 = composeMC 10 normalInit f

main = do
    gen <- getStdGen
    print "normal10"
    print $ runMCN 100000 normal10 gen
