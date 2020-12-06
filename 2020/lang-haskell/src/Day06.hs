module Day06 (solve1, solve2) where

import qualified Data.Set as Set
import Data.List.Split (splitOn)

solve1 :: String -> Int
solve1 = sum . map (Set.size . Set.fromList . filter (/= '\n')) . splitOn "\n\n"

solve2 :: String -> Int
solve2 = sum . map tallyGroup . splitOn "\n\n"

tallyGroup :: String -> Int
tallyGroup =
  Set.size . foldl1 Set.intersection . map Set.fromList . lines