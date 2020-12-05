module Day05 (solve1, solve2) where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (sort)

solve1 :: String -> Int
solve1 = maximum . map findSeat . lines

solve2 :: String -> Int
solve2 = findGap .  sort . map findSeat . lines

findSeat :: String -> Int
findSeat instructions =
  row * 8 + column
  where
    row = fromMaybe 0 $ listToMaybe $
        foldl
        (\acc char ->
          case char of
            'F' -> take (length acc `div` 2) acc
            'B' -> drop (length acc `div` 2) acc
            _ -> acc
        )
        [0..127]
        (take 7 instructions)
    column = fromMaybe 0 $ listToMaybe $
      foldl
        (\acc char ->
          case char of
            'L' -> take (length acc `div` 2) acc
            'R' -> drop (length acc `div` 2) acc
            _ -> acc
        )
        [0..7]
        (drop 7 instructions)

findGap :: [Int] -> Int
findGap (a:b:rest) = if b - a == 2 then b - 1 else findGap (b:rest)
findGap _ = -1
