module Ch23.Random2 where

import Ch23.Random (Die, intToDie)
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

rollDie :: State StdGen Die
rollDie =
  state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> (Int, [Die])
rollsToGetTwenty = rollsToGetN 20

rollsToGetN :: Int -> StdGen -> (Int, [Die])
rollsToGetN n g = go 0 (0, []) g
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum (count, rolls) gen
      | sum >= n = (count, rolls)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) ((count + 1), rolls ++ [intToDie die]) nextGen
