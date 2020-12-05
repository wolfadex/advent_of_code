{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Day04 (solve1, solve2) where

-- import Text.Parsec (parse, Parsec, (<|>))
-- import Text.Parsec.Char (oneOf, digit, letter, string)
-- import Text.Parsec.Combinator (many1, eof)
-- import Control.Monad (void)
import Control.Applicative ((<*), (<|>), (<*>), (<$>), Alternative)
import Control.Monad (void)
import Control.Monad.Combinators (many)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, try, eof, parseError, ParseError(..))
import Text.Megaparsec.Char (space, space1, alphaNumChar, printChar, char, string)
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

---- PARSERS ----

parsePassports :: Parser PassportField -> String -> Maybe Passport
parsePassports fieldParser =
    parseMaybe (parsePassport fieldParser)


parsePassport :: Parser PassportField -> Parser Passport
parsePassport fieldParser = do
  fields <- many (fieldParser <* space1)
  let birthYear = get (\case
                        BirthYear y -> Just y
                        _ -> Nothing
                    ) (trace "carl" fields)
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
parsePassportField = try parseBirthYear
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
  val <- valueParser
  void (space <|> eof)
  return val


parseBirthYear :: Parser PassportField
parseBirthYear = BirthYear <$> parseField "byr" decimal

parseIssueYear :: Parser PassportField
parseIssueYear = IssueYear <$> parseField "iyr" decimal

parseExpirationYear :: Parser PassportField
parseExpirationYear = ExpirationYear <$> parseField "eyr" decimal

parseHeight :: Parser PassportField
parseHeight = Height <$> parseField "hgt" (many alphaNumChar)

parseHairColor :: Parser PassportField
parseHairColor = HairColor <$> parseField "hcl" (many printChar)

parseEyeColor :: Parser PassportField
parseEyeColor = EyeColor <$> parseField "ecl" (many printChar)

parsePassportId :: Parser PassportField
parsePassportId = PassportId <$> parseField "pid" (many printChar)

parseCountryId :: Parser PassportField
parseCountryId = CountryId <$> parseField "cid" (many printChar)

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
