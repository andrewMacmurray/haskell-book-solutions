module Ch23.Random where

import System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x


rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomDieIndex s
      (d2, s2) = randomDieIndex s1
      (d3, _) = randomDieIndex s2
  (intToDie d1, intToDie d2, intToDie d3)


randomDieIndex :: StdGen -> (Int, StdGen)
randomDieIndex = randomR (1, 6)
