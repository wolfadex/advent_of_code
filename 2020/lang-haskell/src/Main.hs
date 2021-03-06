module Main (main) where

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified System.Environment

main :: IO ()
main = do
    args <- System.Environment.getArgs
    case args of
        [inputFilePath, day, part] -> do
            input <- readFile inputFilePath
            case getSolver day (decodePart part) of
                Just solver -> print $ (<>) "Answer: " $ show $ solver input
                Nothing -> putStrLn "Bad args"
        _ -> do
            putStrLn "Expected 3 args: path to input file, day, part" 


data Part = One | Two

decodePart :: String -> Maybe Part
decodePart "1" = Just One
decodePart "2" = Just Two
decodePart _ = Nothing


getSolver :: String -> Maybe Part -> Maybe (String -> Int)
getSolver "01" (Just One) = Just Day01.solve1
getSolver "01" (Just Two) = Just Day01.solve2
getSolver "02" (Just One) = Just Day02.solve1
getSolver "02" (Just Two) = Just Day02.solve2
getSolver "03" (Just One) = Just Day03.solve1
getSolver "03" (Just Two) = Just Day03.solve2
getSolver "04" (Just One) = Just Day04.solve1
getSolver "04" (Just Two) = Just Day04.solve2
getSolver "05" (Just One) = Just Day05.solve1
getSolver "05" (Just Two) = Just Day05.solve2
getSolver "06" (Just One) = Just Day06.solve1
getSolver "06" (Just Two) = Just Day06.solve2
getSolver "07" (Just One) = Just Day07.solve1
getSolver "07" (Just Two) = Just Day07.solve2
getSolver "08" (Just One) = Just Day08.solve1
getSolver "08" (Just Two) = Just Day08.solve2
getSolver "09" (Just One) = Just Day09.solve1
getSolver "09" (Just Two) = Just Day09.solve2
getSolver "10" (Just One) = Just Day10.solve1
getSolver "10" (Just Two) = Just Day10.solve2
getSolver "11" (Just One) = Just Day11.solve1
getSolver "11" (Just Two) = Just Day11.solve2
getSolver "12" (Just One) = Just Day12.solve1
getSolver "12" (Just Two) = Just Day12.solve2
getSolver _ _ = Nothing