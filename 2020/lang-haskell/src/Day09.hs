module Day09 (solve1, solve2) where

import Data.List (sort, sum)

solve1 :: String -> Int
solve1 = findNoSum . map (read :: String -> Int) . lines

findNoSum :: [Int] -> Int
findNoSum nums =
  case drop 25 nums of
    [] -> -1
    a:_ ->
      if isSum preamble a
        then findNoSum $ drop 1 nums
        else a
  where
    preamble = sort $ take 25 nums

isSum :: [Int] -> Int -> Bool
isSum [] _ = False
isSum (a:rest) toFind = member (toFind - a) rest || isSum rest toFind

member :: (Eq a) => a -> [a] -> Bool
member x [] = False
member x (y:ys) | x==y = True
                | otherwise = member x ys

solve2 :: String -> Int
solve2 input =
  findListSum noSumNum (take 2 nums) (drop 2 nums)
  where
    nums = map (read :: String -> Int) $ lines input
    noSumNum = findNoSum nums

findListSum :: Int -> [Int] -> [Int] -> Int
findListSum _ [] [] = -1
findListSum _ [_] [] = -1
findListSum goal [] remainingNums = findListSum goal (take 2 remainingNums) (drop 2 remainingNums)
findListSum goal [a] remainingNums = findListSum goal(a : take 1 remainingNums) (drop 1 remainingNums)
findListSum goal toSumNums remainingNums
  | sum toSumNums == goal =  minimum toSumNums + maximum toSumNums
  | sum toSumNums > goal = findListSum goal (drop 1 toSumNums) remainingNums
  | otherwise = findListSum goal (toSumNums ++ take 1 remainingNums) (drop 1 remainingNums)
