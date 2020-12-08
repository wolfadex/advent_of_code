{-# LANGUAGE NamedFieldPuns #-}

module Day07 (solve1, solve2) where

import Control.Applicative ((<*), (<*>))
import Control.Applicative.Combinators (someTill, choice, sepBy)
import Control.Monad (void)
import Control.Monad.Combinators (many, between)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, eof)
import Text.Megaparsec.Char (space, space1, char, string)
import Text.Megaparsec.Char.Lexer (decimal, charLiteral)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

solve1 :: String -> Int
solve1 input =
  Set.size allParents
  where
    allParents = calculateParents (ColorId "shiny gold") bags
    bags = parseInput input

parentBags :: Map ColorId Bag -> ColorId -> Set ColorId
parentBags bags childId =
    foldl
        (\acc Bag{ bagChildren, bagColor } ->
            let containsChild = (> 0) $  length $ filter (\( _, id ) -> id == childId) bagChildren
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
solve2 = calculateContains (ColorId "shiny gold") . parseInput

calculateContains :: ColorId -> Map ColorId Bag -> Int
calculateContains parentId bags =
  let children = maybe [] bagChildren $ Map.lookup parentId bags
  in
  foldl
    (\total ( count, id ) ->
      total + count + count * calculateContains id bags
    )
    0
    children

---- TYPES ----

newtype ColorId = ColorId String
  deriving (Eq, Ord, Show)

data Bag = Bag
  { bagColor :: ColorId
  , bagChildren :: [(Int, ColorId)]
  }
  deriving (Show)

---- PARSERS ----

type Parser = Parsec Void String

parseInput :: String -> Map ColorId Bag
parseInput = fromMaybe Map.empty . parseMaybe parsebags

parsebags :: Parser (Map ColorId Bag)
parsebags = do
  bags <- many parseBag
  return $ Map.fromList $ map (\bag -> (bagColor bag, bag)) bags

parseBag :: Parser Bag
parseBag = do
  color <- parseColor
  void space
  children <- choice [[] <$ string "contain no other bags.", parseChildren]
  void $ choice [() <$ char '\n', eof]
  return $ Bag color children

parseColor :: Parser ColorId
parseColor = do
  firstPart <- someTill charLiteral (char ' ')
  secondPart <- someTill charLiteral (char ' ')
  void $ choice [string "bags" , string "bag"]
  return $ ColorId $ firstPart <> " " <> secondPart

parseChildren :: Parser [(Int, ColorId)]
parseChildren = between (string "contain") (char '.') (sepBy parseChild (char ','))

parseChild :: Parser (Int, ColorId)
parseChild = (,) <$ space <*> decimal <* space1 <*> parseColor