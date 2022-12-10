{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.List (unfoldr, foldl', stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Text.Megaparsec (Parsec, some, parseMaybe)
import Text.Megaparsec.Char (digitChar, string)

ccw :: [[a]] -> [[a]]
ccw xs =
  let n = length (head xs)
  in foldr (\cs acc -> zipWith ($) ((:) <$> cs) acc) (replicate n []) xs

takeEvery :: Int -> [a] -> [a]
takeEvery n =
  unfoldr (\case
              [] -> Nothing
              x:xs' -> Just (x, drop (n - 1) xs'))

moveP :: Parsec String String (Int, Int, Int)
moveP = do
  string "move "
  n <- read <$> some digitChar
  string " from "
  from <- read <$> some digitChar
  string " to "
  to <- read <$> some digitChar
  pure (n, from - 1, to - 1)

input = do
  raw <- readFile "input"
  let [diagram, moves] = splitOn "\n\n" raw
      cs = [ cs
           | l <- init . lines $ diagram
           , let cs = takeEvery 4 . drop 1 $ l
           ]
      cs't = ccw cs
  pure (map (filter (/= ' ')) cs't, map (fromJust . parseMaybe moveP) . lines $ moves)

updateAt :: Int -> (a -> a)  -> [a] -> [a]
updateAt idx f xs =
  let
    target : rest = drop idx xs
  in
    take idx xs ++ (f target) : rest

setAt :: Int -> a -> [a] -> [a]
setAt idx x = updateAt idx (const x)

exec :: [String] -> (Int, Int, Int) -> [String]
exec st (n, from, to) =
  let
    (xs, from') = splitAt n (st !! from)
    to' = reverse xs ++ (st !! to)
  in
    setAt from from' . setAt to to' $ st

part1 :: ([[Char]], [(Int, Int, Int)]) -> [Char]
part1 = map head . uncurry (foldl' exec)

exec2 :: [String] -> (Int, Int, Int) -> [String]
exec2 st (n, from, to) =
  let
    (xs, from') = splitAt n (st !! from)
    to' = xs ++ (st !! to)
  in
    setAt from from' . setAt to to' $ st

part2 :: ([[Char]], [(Int, Int, Int)]) -> [Char]
part2 = map head . uncurry (foldl' exec2)
