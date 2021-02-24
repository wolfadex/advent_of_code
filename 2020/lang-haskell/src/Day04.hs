{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Day04 (solve1, solve2) where

import Control.Applicative ((<*), (<|>), (<$>), Alternative)
import Control.Monad (void)
import Control.Monad.Combinators (many, manyTill, count)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, try, eof, parseError, ParseError(..))
import Text.Megaparsec.Char (space1, asciiChar, newline, digitChar, char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ErrorFancy(..))
import qualified Data.Set as Set

solve1 :: String -> Int
solve1 = length . mapMaybe (parsePassports parsePassportField) . splitOn "\n\n"

solve2 :: String -> Int
solve2 = length . mapMaybe (parsePassports parsePassportFieldBetter) . splitOn "\n\n"

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

---- PARSERS ----

parsePassports :: Parser PassportField -> String -> Maybe Passport
parsePassports fieldParser =
    parseMaybe (parsePassport fieldParser)


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
      badPlaceholderError

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

parsePassportFieldBetter :: Parser PassportField
parsePassportFieldBetter =
      try parseBirthYearBetter
  <|> try parseIssueYearBetter
  <|> try parseExpirationYearBetter
  <|> try parseHeightBetter
  <|> try parseHairColorBetter
  <|> try parseEyeColorBetter
  <|> try parsePassportIdBetter
  <|> parseCountryId

parseBirthYearBetter :: Parser PassportField
parseBirthYearBetter = do
  year <- parseField "byr" decimal <* sep
  if (year < 1920) || (year > 2002) then
    badPlaceholderError
  else
    return $ BirthYear year

parseIssueYearBetter :: Parser PassportField
parseIssueYearBetter = do
  year <- parseField "iyr" decimal <* sep
  if (year < 2010) || (year > 2020) then
    badPlaceholderError
  else
    return $ IssueYear year

parseExpirationYearBetter :: Parser PassportField
parseExpirationYearBetter = do
  year <- parseField "eyr" decimal <* sep
  if (year < 2020) || (year > 2030) then
    badPlaceholderError
  else
    return $ ExpirationYear year

parseHeightBetter :: Parser PassportField
parseHeightBetter = do
  val <- parseField "hgt" decimal
  hgt <- parseHeightUnit
  void sep
  case hgt of
    Centimeter ->
      if (val < 150) || (val > 193) then
        badPlaceholderError
      else
        return $ Height (show val)
    
    Inch ->
      if (val < 59) || (val > 76) then
        badPlaceholderError
      else
        return $ Height (show val)


parseHeightUnit :: Parser HeightVal
parseHeightUnit = parseCentimeter <|> parseInch


parseCentimeter :: Parser HeightVal
parseCentimeter = do
  void $ string "cm"
  return Centimeter


parseInch :: Parser HeightVal
parseInch = do
  void $ string "in"
  return Inch

data HeightVal
  = Centimeter
  | Inch

parseHairColorBetter :: Parser PassportField
parseHairColorBetter = do
  color <- parseField "hcl" parseHexColor
  void sep
  return $ HairColor (show color)

newtype HexColor = HexColor ( Hex, Hex, Hex, Hex, Hex, Hex ) deriving (Show)

data Hex
  = HexZero
  | HexOne
  | HexTwo
  | HexThree
  | HexFour
  | HexFive
  | HexSix
  | HexSeven
  | HexEight
  | HexNine
  | HexA
  | HexB
  | HexC
  | HexD
  | HexE
  | HexF
  deriving (Show)

parseHexColor :: Parser HexColor
parseHexColor = do
  void $ char '#'
  hexes <- count 6 parseHex
  case hexes of
    [r1, r2, g1, g2, b1, b2] ->
      return $ HexColor (r1, r2, g1, g2, b1, b2)
    _ ->
      badPlaceholderError

parseHex :: Parser Hex
parseHex =
      HexZero <$ char '0'
  <|> HexOne <$ char '1'
  <|> HexTwo <$ char '2'
  <|> HexThree <$ char '3'
  <|> HexFour <$ char '4'
  <|> HexFive <$ char '5'
  <|> HexSix <$ char '6'
  <|> HexSeven <$ char '7'
  <|> HexEight <$ char '8'
  <|> HexNine <$ char '9'
  <|> HexA <$ char 'a'
  <|> HexB <$ char 'b'
  <|> HexC <$ char 'c'
  <|> HexD <$ char 'd'
  <|> HexE <$ char 'e'
  <|> HexF <$ char 'f'

parseEyeColorBetter :: Parser PassportField
parseEyeColorBetter = do
  color <- parseField "ecl" parseEyeColorVal
  void sep
  return $ EyeColor color

parseEyeColorVal :: Parser String
parseEyeColorVal =
      try (string "amb")
  <|> try (string "blu")
  <|> try (string "brn")
  <|> try (string "gry")
  <|> try (string "grn")
  <|> try (string "hzl")
  <|> string "oth"

parsePassportIdBetter :: Parser PassportField
parsePassportIdBetter = do
  val <- parseField "pid" (many digitChar)
  void sep
  if length val == 9 then
    return $ PassportId val
  else
    badPlaceholderError


---- HELPERS ----

get :: (a -> Maybe b) -> [a] -> Maybe b
get pred [] = Nothing
get pred (a:rest) =
  case pred a of
    Just b -> Just b
    Nothing -> get pred rest

badPlaceholderError = parseError $ FancyError 0 Set.empty