module Day03 where

import qualified Runner
import qualified Data.List as List

main :: IO ()
main = Runner.run part1 part2

part1 :: String -> String
part1 = show . (\gamma -> binaryToDecimal gamma * binaryToDecimal (binaryFlip gamma)) . fmap common . List.transpose . fmap toBinary . lines


part2 :: String -> String
-- part2 = show . undefined . fmap read . lines
part2 _ = "TODO"


toBinary :: [Char] -> [Int]
toBinary = fmap (\char -> case char of '1' -> 1; _ -> 0;)


common :: [Int] -> Int
common ints =
  let (zeros, ones) = foldl (\(z, o) i -> if i == 0 then (z + 1, o) else (z, o + 1)) (0, 0) ints
  in if zeros > ones then 0 else 1
    

binaryFlip :: [Int] -> [Int]
binaryFlip = fmap (\i -> if i == 0 then 1 else 0)


binaryToDecimal :: [Int] -> Int
binaryToDecimal = sum . fmap (\i -> 2 ^ i) . List.findIndices ((==) 1) . reverse