import Data.Sample
import Data.Sample.Lib
import Data.Sample.Types

import qualified Data.Sequence as S

import System.Random

data Prize = Goat | Car
    deriving Eq

doorInit :: Int -> Sampler Prize
doorInit n = uniform $ Car : (take (n-1) $ repeat Goat)

removeDoor :: Prize -> Sampler Prize -> Sampler Prize
removeDoor b sa = let (da, g) = runSample sa $ mkStdGen 0
                      in case da of
                        Uniform l -> uniform $ removeFst b l
                        _ -> error "not monty hall"
                  where removeFst b [] = []
                        removeFst b (x:xs) = if x == b then xs
                                           else x : removeFst b xs

montyHall :: Sampler Prize
montyHall = do
    let doors = doorInit 3
    firstPick <- doors
    let remainingDoors = removeDoor firstPick doors
    removeDoor Goat remainingDoors

main = do
    gen <- newStdGen
    let a = sampleN 100000 montyHall gen
        a' = S.filter (== Car) a
        a'' = S.filter (== Goat) a
    print $ "Number of cars won: " ++ (show $ length a')
    print $ "Number of goats won: " ++ (show $ length a'')
