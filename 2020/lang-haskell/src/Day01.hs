module Day01 (solve1, solve2) where

import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

solve1 :: String -> Int
solve1 = findNumber . partition (< 1010) . mapMaybe (readMaybe :: (String -> Maybe Int)) . splitOn "\n"

findNumber :: ([Int], [Int]) -> Int
findNumber ([], highs) = if 1010 `elem` highs then 1010 ^ 2 else -1
findNumber (a:rest, highs) = if b `elem` highs then a * b else findNumber (rest, highs)
  where
    b = 2020 - a

solve2 :: String -> Int
solve2 = findNums3 . mapMaybe (readMaybe :: (String -> Maybe Int)) . lines

findNums3 :: [Int] -> Int
findNums3 [] = -1
findNums3 (a:rest) =
  case findNums2 (2020 - a) rest of
    Just (b, c) -> a * b * c
    Nothing -> findNums3 rest

findNums2 :: Int -> [Int] -> Maybe (Int, Int)
findNums2 _ [] = Nothing
findNums2 goal (a:rest) = if goal - a `elem` rest then Just (a, goal - a) else findNums2 goal rest

---- HELPERS ----

partition :: (a -> Bool) -> [a] -> ( [a], [a] )
partition fn = foldr (\a (l, r) -> if fn a then (a:l, r) else (l, a:r)) ([], [])