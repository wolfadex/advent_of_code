{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Day04 (solve1, solve2) where

-- import Control.Monad (void)
import Control.Applicative ((<*), (<|>), (<*>), (<$>), Alternative)
import Control.Monad (void)
import Control.Monad.Combinators (many, manyTill)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, parse, try, eof, parseError, ParseError(..))
import Text.Megaparsec.Char (space, space1, alphaNumChar, asciiChar, eol, newline, digitChar, lowerChar, char, string)
import Text.Megaparsec.Char.Lexer (symbol, decimal)
import Text.Megaparsec.Error (ErrorFancy(..))
import qualified Data.Set as Set
import Debug.Trace (trace)

solve1 :: String -> Int
solve1 = length . mapMaybe (parsePassports parsePassportField) . splitOn "\n\n"

solve2 :: String -> Int
-- solve2 = length . mapMaybe (parsePassports parsePassportFieldBetter) . splitOn "\n\n"
solve2 = undefined

---- TYPES ----

type Parser = Parsec Void String

data Passport = Passport
    { passBirthYear :: Int
    , passIssueYear :: Int
    , passExpirationYear :: Int
    , passHeight :: String
    , passHairColor :: String
    , passEyeColor :: String
    , passId :: String
    , passCountryId :: Maybe String
    }

data PassportField
    = BirthYear Int
    | IssueYear Int
    | ExpirationYear Int
    | Height String
    | HairColor String
    | EyeColor String
    | PassportId String
    | CountryId String

-- instance Show PassportField where
--   show (BirthYear i) = "BY: " <> show i
--   show (IssueYear i) = "IY: " <> show i
--   show (ExpirationYear i) = "EY: " <> show i
--   show (Height h) = "H: " <> h
--   show (HairColor hc) = "HC: " <> hc
--   show (EyeColor ec) = "EC: " <> ec
--   show (PassportId pi) = "PI: " <> pi
--   show (CountryId ci) = "CI: " <> ci

---- PARSERS ----

parsePassports :: Parser PassportField -> String -> Maybe Passport
parsePassports fieldParser inp =
    case parse (parsePassport fieldParser) "" inp of
      Right p -> Just p
      Left err -> Nothing


parsePassport :: Parser PassportField -> Parser Passport
parsePassport fieldParser = do
  fields <- many fieldParser
  let birthYear = get (\case
                        BirthYear y -> Just y
                        _ -> Nothing
                    ) fields
  let issueYear = get (\case
                        IssueYear y -> Just y
                        _ -> Nothing
                    ) fields
  let expirationYear = get (\case
                        ExpirationYear y -> Just y
                        _ -> Nothing
                    ) fields
  let height = get (\case
                        Height y -> Just y
                        _ -> Nothing
                    ) fields
  let hairColor = get (\case
                        HairColor y -> Just y
                        _ -> Nothing
                    ) fields
  let eyeColor = get (\case
                        EyeColor y -> Just y
                        _ -> Nothing
                    ) fields
  let passportId = get (\case
                        PassportId y -> Just y
                        _ -> Nothing
                    ) fields
  let countryId = get (\case
                        CountryId y -> Just y
                        _ -> Nothing
                    ) fields
  case (birthYear, issueYear, expirationYear, height, hairColor, eyeColor, passportId) of
    (Just by, Just iy, Just ey, Just h, Just hc, Just ec, Just id) ->
      return $ Passport by iy ey h hc ec id countryId

    _ ->
      parseError $ FancyError 0 Set.empty


-- PART 1

parsePassportField :: Parser PassportField
parsePassportField =
      try parseBirthYear
  <|> try parseIssueYear
  <|> try parseExpirationYear
  <|> try parseHeight
  <|> try parseHairColor
  <|> try parseEyeColor
  <|> try parsePassportId
  <|> parseCountryId

parseField :: String -> Parser a -> Parser a
parseField key valueParser = do
  void $ string key
  void $ char ':'
  valueParser

sep :: Parser ()
sep = space1 <|> myNewline <|> eof

myNewline :: Parser ()
myNewline = do
  void $ char '\n'
  

parseBirthYear :: Parser PassportField
parseBirthYear = BirthYear <$> parseField "byr" decimal <* sep

parseIssueYear :: Parser PassportField
parseIssueYear = IssueYear <$> parseField "iyr" decimal <* sep

parseExpirationYear :: Parser PassportField
parseExpirationYear = ExpirationYear <$> parseField "eyr" decimal <* sep

parseHeight :: Parser PassportField
parseHeight = Height <$> parseField "hgt" (manyTill asciiChar sep)

parseHairColor :: Parser PassportField
parseHairColor = HairColor <$> parseField "hcl" (manyTill asciiChar sep)

parseEyeColor :: Parser PassportField
parseEyeColor = EyeColor <$> parseField "ecl" (manyTill asciiChar sep)

parsePassportId :: Parser PassportField
parsePassportId = PassportId <$> parseField "pid" (manyTill asciiChar sep)

parseCountryId :: Parser PassportField
parseCountryId = CountryId <$> parseField "cid" (manyTill asciiChar sep)

-- PART 2

---- HELPERS ----

optional :: Alternative f => f a -> f (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

get :: (a -> Maybe b) -> [a] -> Maybe b
get pred [] = Nothing
get pred (a:rest) =
  case pred a of
    Just b -> Just b
    Nothing -> get pred rest
