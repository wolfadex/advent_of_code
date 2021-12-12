module Runner
    ( run
    ) where

import qualified System.Environment

run :: (String -> String) -> (String -> String) -> IO ()
run part1 part2 = do
    args <- System.Environment.getArgs
    case args of
        [inputFilePath] -> do
            input <- readFile inputFilePath
            print $ "Part 1: " <> part1 input
            print $ "Part 2: " <> part2 input
        _ -> do
            putStrLn "Expected 1 arg: path to input file"
