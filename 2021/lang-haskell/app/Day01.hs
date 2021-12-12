module Day01 where

import qualified Runner

main :: IO ()
main = Runner.run part1 part2

part1 :: String -> String
part1 = show . solve1 . fmap read . lines


solve1 :: [Int] -> Int
solve1 depths =
  case depths of
    [] -> -1
    a : rest ->
      snd $ foldl (\(prevDepth, increases) depth -> ( depth, if depth > prevDepth then increases + 1 else increases)) (a, 0) rest

part2 :: String -> String
part2 = show . slidingWindow . fmap read . lines


slidingWindow :: [Int] -> Int
slidingWindow depths =
  case depths of
    a : b : c : rest -> slidingWindowHelper (a + b + c) (b : c : rest) 0
    [] -> -1


slidingWindowHelper :: Int -> [Int] -> Int -> Int
slidingWindowHelper previousDepth depths increases =
    case depths of
        a : b : c : rest ->
            let
                depth =
                    a + b + c

                newIncreases =
                    if depth > previousDepth then
                        increases + 1

                    else
                        increases
            in
            slidingWindowHelper depth (b : c : rest) newIncreases

        _ ->
            increases