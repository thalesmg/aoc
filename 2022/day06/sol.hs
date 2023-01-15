import qualified Data.Sequence as S
import Data.List (nub)
import Data.Foldable (toList)

input :: IO String
input = readFile "input"

isStartOf :: Eq a => Int -> S.Seq a -> Bool
isStartOf m xs =
  let
    n = length xs
    xs' = toList xs
  in
    n == m && n == length (nub xs')

isStartOfPacket, isStartOfMessage :: Eq a => S.Seq a -> Bool
isStartOfPacket = isStartOf 4
isStartOfMessage = isStartOf 14

appendR :: Int -> a -> S.Seq a -> S.Seq a
appendR n y xs = case S.viewl xs of
                   S.EmptyL -> S.singleton y
                   x S.:< xs' -> if length xs >= n
                                 then xs' S.|> y
                                 else xs S.|> y

part :: Int -> String -> Int
part m = go 0 S.empty
  where
    go n acc [] = if isStartOfPacket acc
                     then n
                     else error "oops!"
    go n acc (y:ys) = let acc' = appendR m y acc
                      in if isStartOfMessage acc'
                         then n + 1
                         else go (n + 1) acc' ys

part1 :: String -> Int
part1 = part 4

part2 :: String -> Int
part2 = part 14
