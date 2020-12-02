{-# LANGUAGE OverloadedStrings #-}

module Day01 (solve1, solve2) where

import Data.List.Split (splitOn)
import Text.Read (readMaybe)

solve1 :: String -> Int
solve1 = findNumber . partition (< 1010) . filterMap (readMaybe :: (String -> Maybe Int)) . splitOn "\n"

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap fn =
  foldr (\a acc ->
          case fn a of
            Nothing -> acc
            Just b -> b:acc
        )
        []

partition :: (a -> Bool) -> [a] -> ( [a], [a] )
partition fn = foldr (\a (l, r) -> if fn a then (a:l, r) else (l, a:r)) ([], [])

findNumber :: ([Int], [Int]) -> Int
findNumber ([], highs) = if 1010 `elem` highs then 1010 ^ 2 else -1
findNumber (a:rest, highs) = if b `elem` highs then a * b else findNumber (rest, highs)
  where
    b = 2020 - a

solve2 :: String -> Int
solve2 input = -1