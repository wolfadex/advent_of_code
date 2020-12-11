{-# LANGUAGE NamedFieldPuns #-}

module Day08 (solve1, solve2) where

import Control.Applicative ((<|>))
import Control.Applicative.Combinators (choice)
import Data.Maybe (fromMaybe)
import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (string, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array.IArray (Array, (!), (//))
import qualified Data.Array.IArray as Array

solve1 :: String -> Int
solve1 = snd . runProgram Set.empty . Program 0 0 . (\ls -> Array.listArray (0, length ls - 1) ls) . map parseInstructions . lines

runProgram :: Set Int -> Program -> (Bool, Int)
runProgram priorInstructions prog@Program { progCount, progPointer, progOperations }
  | progPointer > upperSize = (False, progCount)
  | Set.member progPointer priorInstructions = (True, progCount)
  | otherwise =
      case nextOp of
            NoOp _ ->
              runProgram
                nextPriors
                prog { progPointer = progPointer + 1 }
            Accumulate amount ->
              runProgram
                nextPriors
                prog { progPointer = progPointer + 1, progCount = progCount + amount }
            Jump direction ->
              runProgram
                nextPriors
                prog { progPointer = progPointer + direction }
  where
    nextOp = progOperations ! progPointer
    nextPriors = Set.insert progPointer priorInstructions
    (_,upperSize) = Array.bounds progOperations

solve2 :: String -> Int
solve2 = findProgram . Program 0 0 . (\ls -> Array.listArray (0, length ls - 1) ls) . map parseInstructions . lines

findProgram :: Program -> Int
findProgram prog = findProgramHelper prog (-1) prog


findProgramHelper :: Program -> Int -> Program -> Int
findProgramHelper baseProg previousFlipped progToRun =
  case runProgram Set.empty progToRun of
    (False, count) -> count
    (True, _) ->
      findProgramHelper baseProg (fst nextFlipped) nextProgToRun
      where
        nextFlipped = findNextToFlip previousFlipped (progOperations baseProg)
        nextProgToRun = baseProg { progOperations = progOperations baseProg // [nextFlipped]}

findNextToFlip :: Int -> Array Int Operation -> (Int, Operation)
findNextToFlip prevFlipped ops =
  case op of
    NoOp amount -> (toTry, Jump amount)
    Jump amount -> (toTry, NoOp amount)
    Accumulate _ -> findNextToFlip toTry ops
  where
    toTry = prevFlipped + 1
    op = ops ! toTry

---- TYPES ----

data Program = Program
  { progCount :: Int
  , progPointer :: Int
  , progOperations :: Array Int Operation
  }
  deriving (Show)


data Operation
  = NoOp Int
  | Accumulate Int
  | Jump Int
  deriving (Show)


parseInstructions :: String -> Operation
parseInstructions =
  fromMaybe (NoOp 0) . parseMaybe parseOp

type Parser = Parsec Void String

parseOp :: Parser Operation
parseOp = parseNoOp <|> parseAccumulate <|> parseJump

parseNoOp :: Parser Operation
parseNoOp = do
  void $ string "nop "
  sign <- choice [char '+', char '-' ]
  amount <- decimal
  return $ NoOp $ if sign == '-' then -amount else amount

parseAccumulate :: Parser Operation
parseAccumulate = do
  void $ string "acc "
  sign <- choice [char '+', char '-' ]
  amount <- decimal
  return $ Accumulate $ if sign == '-' then -amount else amount

parseJump :: Parser Operation
parseJump =  do
  void $ string "jmp "
  sign <- choice [char '+', char '-' ]
  amount <- decimal
  return $ Jump $ if sign == '-' then -amount else amount