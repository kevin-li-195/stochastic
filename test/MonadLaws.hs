import Data.Stochastic
import Data.Stochastic.Internal
import Data.Stochastic.Types

import System.Random
import System.Exit

f :: Double -> Sampler Double
f d = if d == 3 then normal 5 2
      else normal 4 1

g :: Double -> Sampler Double
g d = if d < 10 then normal 33 10
      else normal 2.5 0.5

leftId :: Sampler Double
leftId = f 3

leftId' :: Sampler Double
leftId' = return 3 >>= f

rightId :: Sampler Double
rightId = normal 10 3

rightId' :: Sampler Double
rightId' = rightId >>= return

rightId'' :: Sampler Double
rightId'' = rightId' >>= return

rightId''' :: Sampler Double
rightId''' = rightId'' >>= return

assoc :: Sampler Double
assoc = (rightId >>= f) >>= g

assoc' :: Sampler Double
assoc' = rightId >>= (\x -> f x >>= g)

main :: IO Int
main = do
    gen <- getStdGen
    let l = sampleN 1000 leftId gen
        l'= sampleN 1000 leftId' gen
        r = sampleN 1000 rightId gen
        r'= sampleN 1000 rightId' gen
        r'' = sampleN 1000 rightId'' gen
        r'''= sampleN 1000 rightId''' gen
        a = sampleN 1000 assoc gen
        a'= sampleN 1000 assoc' gen
    print "Left"
    print l
    print l'
    print "Right"
    print r
    print r'
    print "Assoc"
    print a
    print a'
    
    if (l /= l') || (r' /= r'') || (r /= r''') || (r /= r') || (r'' /= r''') || (a /= a') then exitFailure
    else return 0
