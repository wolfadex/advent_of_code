module Day10 (solve1, solve2) where

import Data.List (sort)

solve1 :: String -> Int
solve1 = findDiffs (0, 0) . (:) 0 . sort . (\ls -> maximum ls + 3 : ls) . map read . lines

findDiffs :: (Int, Int) -> [Int] -> Int
findDiffs (ones, threes) [] =  ones * threes
findDiffs (ones, threes) [a, b] = findDiffs (if b - a == 1 then ones + 1 else ones, if b - a == 3 then threes + 1 else threes) []
findDiffs (ones, threes) (a:b:rest) =
  findDiffs (newOnes, newThrees) (b:rest)
  where
    newOnes = if b - a == 1 then ones + 1 else ones
    newThrees = if b - a == 3 then threes + 1 else threes

solve2 :: String -> Int
solve2 = findPermutations . (:) 0 . sort . map read . lines

findPermutations :: [Int] -> Int
findPermutations =
  product
    . map 
      (\g ->
        case length g of
          2 -> 1
          3 -> 2
          5 -> 7
          l -> l
      )
    . chunk


chunk :: [Int] -> [[Int]]
chunk nums =
   (\(_, res, other) -> other : res)
    $ foldr
      (\n (prev, combos, combo) ->
        if prev - n == 3
          then (n, combo : combos, [n])
          else (n, combos, n : combo)
      )
      (largest, [], [largest])
      nums
  where
    largest = maximum nums + 3
