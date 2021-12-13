module Day03 where

import qualified Runner
import qualified Data.List as List

main :: IO ()
main = Runner.run part1 part2

part1 :: String -> String
part1 = show . (\gamma -> binaryToDecimal gamma * binaryToDecimal (binaryFlip gamma)) . fmap common . List.transpose . fmap stringToBinary . lines


common :: [Int] -> Int
common ints =
  let (zeros, ones) = foldl (\(z, o) i -> if i == 0 then (z + 1, o) else (z, o + 1)) (0, 0) ints
  in if zeros > ones then 0 else 1


part2 :: String -> String
part2 = show . (\binaries -> (binaryToDecimal $ mostCommon 0 binaries) * (binaryToDecimal $ leastCommon 0 binaries)) . fmap stringToBinary . lines


mostCommon :: Int -> [[Int]] -> [Int]
mostCommon index remainingBinaries =
  case List.filter (\bits -> (bits !! index) == keepN) remainingBinaries of
    [answer] -> answer
    [] -> undefined
    remaining -> mostCommon (index + 1) remaining
  where
    indexedBits = fmap (\bits -> bits !! index) remainingBinaries
    (zeros, ones) = foldr (\i (z, o) -> if i == 0 then (z + 1, o) else (z, o + 1)) (0, 0) indexedBits
    keepN = if zeros > ones then 0 else 1


leastCommon :: Int -> [[Int]] -> [Int]
leastCommon index remainingBinaries =
  case List.filter (\bits -> (bits !! index) == keepN) remainingBinaries of
    [answer] -> answer
    [] -> undefined
    remaining -> leastCommon (index + 1) remaining
  where
    indexedBits = fmap (\bits -> bits !! index) remainingBinaries
    (zeros, ones) = foldr (\i (z, o) -> if i == 0 then (z + 1, o) else (z, o + 1)) (0, 0) indexedBits
    keepN = if zeros > ones then 1 else 0


stringToBinary :: [Char] -> [Int]
stringToBinary = fmap (\char -> case char of '1' -> 1; _ -> 0;)
    

binaryFlip :: [Int] -> [Int]
binaryFlip = fmap (\i -> if i == 0 then 1 else 0)


binaryToDecimal :: [Int] -> Int
binaryToDecimal = sum . fmap (\i -> 2 ^ i) . List.findIndices ((==) 1) . reverse