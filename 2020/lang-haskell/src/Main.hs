module Main (main) where

import qualified Day01
import qualified Day02
import qualified Day03
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
getSolver _ _ = Nothing