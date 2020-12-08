{-# LANGUAGE NamedFieldPuns #-}

module Day07 (solve1, solve2) where

import Control.Applicative ((<*), (<|>), (<*>), (<$>), Alternative)
import Control.Applicative.Combinators (someTill, choice)
import Control.Monad (void)
import Control.Monad.Combinators (many, between)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, try, eof, parseError, ParseError(..), takeWhileP)
import Text.Megaparsec.Char (space, space1, alphaNumChar, printChar, char, string)
import Text.Megaparsec.Char.Lexer (symbol, decimal, charLiteral)
import Text.Megaparsec.Error (ErrorFancy(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

solve1 :: String -> Int
solve1 = Set.size . calculateParents (ColorId "shiny gold") . parseInput

parentBags :: Map ColorId Bag -> ColorId -> Set ColorId
parentBags bags childId =
    foldl
        (\acc Bag{ bagChildren, bagColor } ->
            let containsChild = (> 0) $ length $ filter (\( _, id ) -> id == childId) bagChildren
            in if containsChild then Set.insert bagColor acc else acc
        )
        Set.empty
        bags


calculateParents :: ColorId -> Map ColorId Bag -> Set ColorId
calculateParents childId bags =
  foldr (\parent t -> Set.union (calculateParents parent bags) t) parents (Set.toList parents)
  where
    parents = parentBags bags childId

solve2 :: String -> Int
solve2 = undefined

---- TYPES ----

newtype ColorId = ColorId String
  deriving (Eq, Ord)

data Bag = Bag
  { bagColor :: ColorId
  , bagChildren :: [(Int, ColorId)]
  }

---- PARSERS ----

type Parser = Parsec Void String

parseInput :: String -> Map ColorId Bag
parseInput =
  fromMaybe Map.empty . parseMaybe parsebags

parsebags :: Parser (Map ColorId Bag)
parsebags = do
  bags <- many parseBag
  void eof
  return $ Map.fromList $ map (\bag -> (bagColor bag, bag)) bags

parseBag :: Parser Bag
parseBag = do
  color <- parseColor
  void space
  children <- choice [[] <$ string "contain no other bags.", parseChildren]
  return $ Bag color children

parseColor :: Parser ColorId
parseColor = do
  firstPart <- someTill charLiteral (char ' ')
  void space1
  secondPart <- someTill charLiteral (char ' ')
  void space1
  void $ choice [string "bags" , string "bag"]
  return $ ColorId $ firstPart <> " " <> secondPart

parseChildren :: Parser [(Int, ColorId)]
parseChildren = between (string "contain") (char '.') (many parseChild)

parseChild :: Parser (Int, ColorId)
parseChild = (,) <$ space <*> decimal <* space <*> parseColor <* try (char ',')