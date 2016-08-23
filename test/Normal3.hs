import Data.Sample
import Data.Sample.Types
import Data.Sample.Lib

import System.Random

normalInit :: StochProcess
normalInit = normalMC 10 3

f :: Double -> StochProcess
f d = if d < 7 then normalMC (0.9 * d) (0.3 * d)
  else if d > 13 then normalMC (1.1 * d) (0.3 * d)
  else normalMC d (0.3 * d)

normal3 :: StochProcess
normal3 = composeMC 3 normalInit f

main = do
    gen <- newStdGen
    print "normal3"
    print $ runProcessN 100000 normal3 gen
