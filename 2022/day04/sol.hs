{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

input :: IO [((Int, Int), (Int, Int))]
input = do
  ls <- T.lines <$> TIO.readFile "input"
  pure [ ((l1, r1), (l2, r2))
       | l <- ls
       , let [[l1, r1], [l2, r2]] = map (map (read . T.unpack) . T.splitOn "-") . T.splitOn "," $ l
       ]

isFullyContainedIn :: (Int, Int) -> (Int, Int) -> Bool
(l1, r1) `isFullyContainedIn` (l2, r2) = l1 >= l2 && r1 <= r2

overlaps :: (Int, Int) -> (Int, Int) -> Bool
--(l1, r1) `overlaps` (l2, r2) = (l1 <= l2 <= r1) || (l2 <= r1 && r1 <= r2)
(l1, r1) `overlaps` (l2, r2) = (l2 <= r1 && l1 <= r2) || (l1 <= r2 && l2 <= r1)

part1 :: [((Int, Int), (Int, Int))] -> Int
part1 inp = length [1 | (l, r) <- inp, l `isFullyContainedIn` r || r `isFullyContainedIn` l]

part2 :: [((Int, Int), (Int, Int))] -> Int
part2 = length . filter (uncurry overlaps)
