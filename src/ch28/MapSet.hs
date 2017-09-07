module Ch28.MapSet where

import Criterion.Main
import qualified Data.Set as S
import qualified Data.Map as M


bumpIt :: (Int, Int) -> (Int, Int)
bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

s2 :: S.Set Int
s2 = S.fromList $ take 5000 stream
  where stream = iterate (+1) 6000

m2 :: M.Map Int Int
m2 = M.fromList $ take 5000 stream
  where stream = iterate bumpIt (5000, 5000)


membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s


insertMap :: Int -> Int -> M.Map Int Int
insertMap k v = M.insert k v m

insertSet :: Int -> S.Set Int
insertSet i = S.insert i s


unionSet :: S.Set Int -> S.Set Int
unionSet s' = S.union s s'

unionMap :: M.Map Int Int -> M.Map Int Int
unionMap m' = M.union m m'


main :: IO ()
main = defaultMain
  [ bench "member check map" $
    whnf membersMap 9999
  , bench "member check set" $
    whnf membersSet 9999
  , bench "insert check set" $
    whnf insertSet 10001
  , bench "insert check map" $
    whnf (insertMap 10001) 1
  , bench "union check map" $
    whnf unionMap m2
  , bench "union check set" $
    whnf unionSet s2
  ]
