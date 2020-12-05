port module Day04 exposing (main)

import Parser exposing ((|.), (|=), Parser, Step(..))
import Platform


main : Program String Model Msg
main =
    Platform.worker
        { init = init
        , subscriptions = subscriptions
        , update = update
        }


type alias Model =
    { input : String
    }


init : String -> ( Model, Cmd Msg )
init input =
    ( { input = input }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ part1 (\_ -> PartOne)
        , part2 (\_ -> PartTwo)
        ]


port part1 : (String -> msg) -> Sub msg


port part2 : (String -> msg) -> Sub msg


port response : String -> Cmd msg


type Msg
    = PartOne
    | PartTwo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PartOne ->
            ( model
            , model.input
                |> String.split "\n\n"
                |> List.filterMap (parsePassports parsePassportField)
                |> List.length
                |> String.fromInt
                |> (++) "Answer: "
                |> response
            )

        PartTwo ->
            ( model
            , model.input
                |> String.split "\n\n"
                |> List.filterMap (parsePassports parsePassportFieldBetter)
                |> List.length
                |> String.fromInt
                |> (++) "Answer: "
                |> response
            )


parsePassports : Parser PassportField -> String -> Maybe Passport
parsePassports fieldParser str =
    Parser.run (parsePassport fieldParser) str
        |> Result.toMaybe


type alias Passport =
    { birthYear : Int
    , issueYear : Int
    , expirationYear : Int
    , height : String
    , hairColor : String
    , eyeColor : String
    , id : String
    , countryId : Maybe String
    }



---- PARSERS ----


parsePassport : Parser PassportField -> Parser Passport
parsePassport fieldParser =
    Parser.loop [] (parsePassportHelper fieldParser)
        |> Parser.andThen
            (\fields ->
                Just Passport
                    |> Maybe.andThen
                        (\f ->
                            get
                                (\field ->
                                    case field of
                                        BirthYear year ->
                                            Just year

                                        _ ->
                                            Nothing
                                )
                                fields
                                |> Maybe.map f
                        )
                    |> Maybe.andThen
                        (\f ->
                            get
                                (\field ->
                                    case field of
                                        IssueYear year ->
                                            Just year

                                        _ ->
                                            Nothing
                                )
                                fields
                                |> Maybe.map f
                        )
                    |> Maybe.andThen
                        (\f ->
                            get
                                (\field ->
                                    case field of
                                        ExpirationYear year ->
                                            Just year

                                        _ ->
                                            Nothing
                                )
                                fields
                                |> Maybe.map f
                        )
                    |> Maybe.andThen
                        (\f ->
                            get
                                (\field ->
                                    case field of
                                        Height height ->
                                            Just height

                                        _ ->
                                            Nothing
                                )
                                fields
                                |> Maybe.map f
                        )
                    |> Maybe.andThen
                        (\f ->
                            get
                                (\field ->
                                    case field of
                                        HairColor color ->
                                            Just color

                                        _ ->
                                            Nothing
                                )
                                fields
                                |> Maybe.map f
                        )
                    |> Maybe.andThen
                        (\f ->
                            get
                                (\field ->
                                    case field of
                                        EyeColor color ->
                                            Just color

                                        _ ->
                                            Nothing
                                )
                                fields
                                |> Maybe.map f
                        )
                    |> Maybe.andThen
                        (\f ->
                            get
                                (\field ->
                                    case field of
                                        PassportId id ->
                                            Just id

                                        _ ->
                                            Nothing
                                )
                                fields
                                |> Maybe.map f
                        )
                    |> Maybe.map
                        (\f ->
                            get
                                (\field ->
                                    case field of
                                        CountryId id ->
                                            Just id

                                        _ ->
                                            Nothing
                                )
                                fields
                                |> f
                        )
                    |> (\p ->
                            case p of
                                Just pp ->
                                    Parser.succeed pp

                                Nothing ->
                                    Parser.problem "invalid passport"
                       )
            )


parsePassportHelper : Parser PassportField -> List PassportField -> Parser (Step (List PassportField) (List PassportField))
parsePassportHelper fieldParser fields =
    Parser.oneOf
        [ Parser.succeed (\field -> Loop (field :: fields))
            |= fieldParser
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Done fields)
        ]


type PassportField
    = BirthYear Int
    | IssueYear Int
    | ExpirationYear Int
    | Height String
    | HairColor String
    | EyeColor String
    | PassportId String
    | CountryId String



-- PART 1


parsePassportField : Parser PassportField
parsePassportField =
    [ parseBirthYear
    , parseIssueYear
    , parseExpirationYear
    , parseHeight
    , parseHairColor
    , parseEyeColor
    , parsePassportId
    , parseCountryId
    ]
        |> List.map Parser.backtrackable
        |> Parser.oneOf


parseBirthYear : Parser PassportField
parseBirthYear =
    parseField
        |> Parser.andThen
            (\( name, value ) ->
                case ( name, String.toInt value ) of
                    ( "byr", Just year ) ->
                        Parser.succeed (BirthYear year)

                    ( _, Nothing ) ->
                        Parser.problem ("Expected a year but got: " ++ value)

                    _ ->
                        Parser.problem ("Expected 'byr' but got: " ++ name)
            )


parseIssueYear : Parser PassportField
parseIssueYear =
    parseField
        |> Parser.andThen
            (\( name, value ) ->
                case ( name, String.toInt value ) of
                    ( "iyr", Just year ) ->
                        Parser.succeed (IssueYear year)

                    ( _, Nothing ) ->
                        Parser.problem ("Expected a year but got: " ++ value)

                    _ ->
                        Parser.problem ("Expected 'iyr' but got: " ++ name)
            )


parseExpirationYear : Parser PassportField
parseExpirationYear =
    parseField
        |> Parser.andThen
            (\( name, value ) ->
                case ( name, String.toInt value ) of
                    ( "eyr", Just year ) ->
                        Parser.succeed (ExpirationYear year)

                    ( _, Nothing ) ->
                        Parser.problem ("Expected a year but got: " ++ value)

                    _ ->
                        Parser.problem ("Expected 'eyr' but got: " ++ name)
            )


parseHeight : Parser PassportField
parseHeight =
    parseField
        |> Parser.andThen
            (\( name, value ) ->
                if name == "hgt" then
                    Parser.succeed (Height value)

                else
                    Parser.problem ("Expected 'hgt' but got: " ++ name)
            )


parseHairColor : Parser PassportField
parseHairColor =
    parseField
        |> Parser.andThen
            (\( name, value ) ->
                if name == "hcl" then
                    Parser.succeed (HairColor value)

                else
                    Parser.problem ("Expected 'hcl' but got: " ++ name)
            )


parseEyeColor : Parser PassportField
parseEyeColor =
    parseField
        |> Parser.andThen
            (\( name, value ) ->
                if name == "ecl" then
                    Parser.succeed (EyeColor value)

                else
                    Parser.problem ("Expected 'ecl' but got: " ++ name)
            )


parsePassportId : Parser PassportField
parsePassportId =
    parseField
        |> Parser.andThen
            (\( name, value ) ->
                if name == "pid" then
                    Parser.succeed (PassportId value)

                else
                    Parser.problem ("Expected 'pid' but got: " ++ name)
            )


parseCountryId : Parser PassportField
parseCountryId =
    parseField
        |> Parser.andThen
            (\( name, value ) ->
                if name == "cid" then
                    Parser.succeed (CountryId value)

                else
                    Parser.problem ("Expected 'cid' but got: " ++ name)
            )


parseField : Parser ( String, String )
parseField =
    Parser.succeed Tuple.pair
        |= (Parser.succeed ()
                |. Parser.chompIf (\_ -> True)
                |. Parser.chompIf (\_ -> True)
                |. Parser.chompIf (\_ -> True)
                |> Parser.getChompedString
           )
        |. Parser.symbol ":"
        |= (Parser.succeed ()
                |. Parser.chompWhile (\c -> c /= ' ' && c /= '\n')
                |> Parser.getChompedString
           )



-- PART 2


parsePassportFieldBetter : Parser PassportField
parsePassportFieldBetter =
    [ parseBirthYearBetter
    , parseIssueYearBetter
    , parseExpirationYearBetter
    , parseHeightBetter
    , parseHairColorBetter
    , parseEyeColorBetter
    , parsePassportIdBetter
    , parseCountryId
    ]
        |> List.map Parser.backtrackable
        |> Parser.oneOf


parseBirthYearBetter : Parser PassportField
parseBirthYearBetter =
    parseFieldBetter
        BirthYear
        "byr"
        (Parser.int
            |> Parser.andThen
                (\year ->
                    if year < 1920 then
                        Parser.problem ("Year must be at least 1920, got: " ++ String.fromInt year)

                    else if year > 2002 then
                        Parser.problem ("Year must be no more than 2002, got: " ++ String.fromInt year)

                    else
                        Parser.succeed year
                )
        )


parseIssueYearBetter : Parser PassportField
parseIssueYearBetter =
    parseFieldBetter
        IssueYear
        "iyr"
        (Parser.int
            |> Parser.andThen
                (\year ->
                    if year < 2010 then
                        Parser.problem ("Year must be at least 2010, got: " ++ String.fromInt year)

                    else if year > 2020 then
                        Parser.problem ("Year must be no more than 2020, got: " ++ String.fromInt year)

                    else
                        Parser.succeed year
                )
        )


parseExpirationYearBetter : Parser PassportField
parseExpirationYearBetter =
    parseFieldBetter
        ExpirationYear
        "eyr"
        (Parser.int
            |> Parser.andThen
                (\year ->
                    if year < 2020 then
                        Parser.problem ("Year must be at least 2020, got: " ++ String.fromInt year)

                    else if year > 2030 then
                        Parser.problem ("Year must be no more than 2030, got: " ++ String.fromInt year)

                    else
                        Parser.succeed year
                )
        )


parseHeightBetter : Parser PassportField
parseHeightBetter =
    parseFieldBetter
        (\( amount, unit ) ->
            Height (String.fromInt amount)
        )
        "hgt"
        parseHeightExact


type Unit
    = Inch
    | Centimeter


parseHeightExact : Parser ( Int, Unit )
parseHeightExact =
    Parser.succeed Tuple.pair
        |= Parser.int
        |= parseUnit
        |> Parser.andThen
            (\( amount, unit ) ->
                case unit of
                    Inch ->
                        if amount < 59 then
                            Parser.problem ("Inch height must be at least 59, got: " ++ String.fromInt amount)

                        else if amount > 76 then
                            Parser.problem ("Inch height must be no more than 76, got: " ++ String.fromInt amount)

                        else
                            Parser.succeed ( amount, unit )

                    Centimeter ->
                        if amount < 150 then
                            Parser.problem ("Centimeter height must be at least 150, got: " ++ String.fromInt amount)

                        else if amount > 193 then
                            Parser.problem ("Centimeter height must be no more than 193, got: " ++ String.fromInt amount)

                        else
                            Parser.succeed ( amount, unit )
            )


parseUnit : Parser Unit
parseUnit =
    Parser.oneOf
        [ Parser.symbol "in" |> Parser.map (\_ -> Inch)
        , Parser.symbol "cm" |> Parser.map (\_ -> Centimeter)
        ]


parseHairColorBetter : Parser PassportField
parseHairColorBetter =
    parseFieldBetter
        (\_ -> HairColor "valid color")
        "hcl"
        parseHexColor


type alias HexColor =
    { red : ( Hex, Hex )
    , green : ( Hex, Hex )
    , blue : ( Hex, Hex )
    }


type Hex
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


parseHexColor : Parser HexColor
parseHexColor =
    Parser.succeed HexColor
        |. Parser.symbol "#"
        |= parseHexPair
        |= parseHexPair
        |= parseHexPair


parseHexPair : Parser ( Hex, Hex )
parseHexPair =
    Parser.succeed Tuple.pair
        |= parseHex
        |= parseHex


parseHex : Parser Hex
parseHex =
    Parser.oneOf
        [ Parser.symbol "0" |> Parser.map (\_ -> HexZero)
        , Parser.symbol "1" |> Parser.map (\_ -> HexOne)
        , Parser.symbol "2" |> Parser.map (\_ -> HexTwo)
        , Parser.symbol "3" |> Parser.map (\_ -> HexThree)
        , Parser.symbol "4" |> Parser.map (\_ -> HexFour)
        , Parser.symbol "5" |> Parser.map (\_ -> HexFive)
        , Parser.symbol "6" |> Parser.map (\_ -> HexSix)
        , Parser.symbol "7" |> Parser.map (\_ -> HexSeven)
        , Parser.symbol "8" |> Parser.map (\_ -> HexEight)
        , Parser.symbol "9" |> Parser.map (\_ -> HexNine)
        , Parser.symbol "a" |> Parser.map (\_ -> HexA)
        , Parser.symbol "b" |> Parser.map (\_ -> HexB)
        , Parser.symbol "c" |> Parser.map (\_ -> HexC)
        , Parser.symbol "d" |> Parser.map (\_ -> HexD)
        , Parser.symbol "e" |> Parser.map (\_ -> HexE)
        , Parser.symbol "f" |> Parser.map (\_ -> HexF)
        ]


parseEyeColorBetter : Parser PassportField
parseEyeColorBetter =
    parseFieldBetter
        (\_ -> EyeColor "valid color")
        "ecl"
        parseEyeColorExact


type EyeColor
    = Amber
    | Blue
    | Brown
    | Gray
    | Green
    | Hazel
    | OtherEyeColor


parseEyeColorExact : Parser EyeColor
parseEyeColorExact =
    [ Parser.symbol "amb" |> Parser.map (\_ -> Amber)
    , Parser.symbol "blu" |> Parser.map (\_ -> Blue)
    , Parser.symbol "brn" |> Parser.map (\_ -> Brown)
    , Parser.symbol "gry" |> Parser.map (\_ -> Gray)
    , Parser.symbol "grn" |> Parser.map (\_ -> Green)
    , Parser.symbol "hzl" |> Parser.map (\_ -> Hazel)
    , Parser.symbol "oth" |> Parser.map (\_ -> OtherEyeColor)
    ]
        |> List.map Parser.backtrackable
        |> Parser.oneOf


parsePassportIdBetter : Parser PassportField
parsePassportIdBetter =
    parseFieldBetter
        PassportId
        "pid"
        (Parser.succeed ()
            |. Parser.chompWhile Char.isDigit
            |> Parser.getChompedString
            |> Parser.andThen
                (\possibleId ->
                    if String.length possibleId == 9 then
                        Parser.succeed possibleId

                    else
                        Parser.problem ("Id must be exaclt 9 digits but got: " ++ possibleId)
                )
        )


parseFieldBetter : (a -> b) -> String -> Parser a -> Parser b
parseFieldBetter f key valueParser =
    Parser.succeed f
        |. Parser.symbol key
        |. Parser.symbol ":"
        |= valueParser
        |. Parser.oneOf
            [ Parser.chompIf ((/=) ' ')
            , Parser.chompIf ((/=) '\n')
            , Parser.end
            ]



---- HELPERS ----


get : (a -> Maybe b) -> List a -> Maybe b
get predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            case predicate first of
                Just b ->
                    Just b

                Nothing ->
                    get predicate rest
