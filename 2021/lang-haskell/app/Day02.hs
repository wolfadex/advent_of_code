module Day02 where

import qualified Runner
import qualified Data.Maybe as Maybe

main :: IO ()
main = Runner.run part1 part2

part1 :: String -> String
part1 = show . navigate1 (0, 0) . parseCommands . lines


navigate1 :: (Int, Int) -> [Command] -> Int
navigate1 (depth, x) commands =
  case commands of
    [] -> depth * x
    (Forward n) : rest -> navigate1 ( depth, x + n ) rest
    (Up n) : rest -> navigate1 ( depth - n, x ) rest
    (Down n) : rest -> navigate1 ( depth + n, x ) rest


part2 :: String -> String
part2 = show . navigate2 (0, 0, 0) . parseCommands . lines


navigate2 :: (Int, Int, Int) -> [Command] -> Int
navigate2 (depth, x, aim) commands =
  case commands of
    [] -> depth * x
    (Forward n) : rest -> navigate2 ( depth + n * aim, x + n, aim ) rest
    (Up n) : rest -> navigate2 ( depth, x, aim - n ) rest
    (Down n) : rest -> navigate2 ( depth, x, aim + n ) rest

-- Parsing of commands

data Command = Forward Int | Up Int | Down Int

parseCommands :: [String] -> [Command]
parseCommands =
  Maybe.catMaybes . fmap parseCommand


parseCommand :: String -> Maybe Command
parseCommand str =
  case str of
    'f':'o':'r':'w':'a':'r':'d':' ':n -> Just $ Forward (read n)
    'u':'p':' ':n -> Just $ Up (read n)
    'd':'o':'w':'n':' ':n -> Just $ Down (read n)
    _ -> Nothing