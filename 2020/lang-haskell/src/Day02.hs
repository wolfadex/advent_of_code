{-# LANGUAGE NamedFieldPuns #-}

module Day02 (solve1, solve2) where

import Text.Parsec (parse, Parsec, (<|>))
import Text.Parsec.Char (oneOf, digit, letter, string)
import Text.Parsec.Combinator (many1, eof)
import Control.Monad (void)
import Control.Applicative ( many)

solve1 :: String -> Int
solve1 input =
  case parse parsePossiblePasswords "" input of
    Left err -> -1
    Right possiblePasswords ->
      length $ filterMap validPassword1 possiblePasswords

validPassword1 :: PossiblePassword -> Maybe Password
validPassword1 PossiblePassword { character, content, low, high } =
  if charCount >= low && charCount <= high then Just (Password content) else Nothing
  where
    charCount = length $ indicies character content


solve2 :: String -> Int
solve2 input =
  case parse parsePossiblePasswords "" input of
    Left err -> -1
    Right possiblePasswords ->
      length $ filterMap validPassword2 possiblePasswords

validPassword2 :: PossiblePassword -> Maybe Password
validPassword2 PossiblePassword { character, content, low, high } =
  if hasLow `xor` hasHigh then Just (Password content) else Nothing
    where
      idxs = indicies character content
      hasLow = low - 1 `elem` idxs
      hasHigh = high - 1 `elem` idxs

---- TYPES ----

newtype Password = Password String

data PossiblePassword = PossiblePassword
  { low :: Int
  , high :: Int
  , character :: Char
  , content :: String
  }

---- PARSERS ----

parsePossiblePasswords :: Parsec String () [PossiblePassword]
parsePossiblePasswords = many (parsePossiblePassword <* (whiteSpace <|> eof))

parseInt :: Parsec String () Int
parseInt = do
  d <- many1 digit
  return $ read d

parsePossiblePassword :: Parsec String () PossiblePassword
parsePossiblePassword = do
  l <- parseInt
  void $ symbol "-"
  h <- parseInt
  void whiteSpace
  ch <- letter
  void $ symbol ":"
  void whiteSpace
  cnt <- many1 letter
  return $ PossiblePassword
    { low = l
    , high = h
    , character = ch
    , content = cnt
    }

whiteSpace :: Parsec String () ()
whiteSpace = do
  void $ many $ oneOf " \n\t"

symbol :: String -> Parsec String () ()
symbol = void . string

---- HELPERS ----

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap fn =
  foldr (\a acc ->
          case fn a of
            Nothing -> acc
            Just b -> b:acc
        )
        []

indicies :: Char -> String -> [Int]
indicies ch =
  snd . foldl (\(i, acc) c -> ( i + 1, if c == ch then i:acc else acc )) (0,[])

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False