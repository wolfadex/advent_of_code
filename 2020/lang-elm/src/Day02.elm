port module Day02 exposing (main)

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
            , case Parser.run parsePossiblePasswords model.input of
                Ok possibelPasswords ->
                    possibelPasswords
                        |> List.filterMap
                            (\{ low, high, character, text } ->
                                let
                                    charCount =
                                        String.indices (String.fromChar character) text
                                            |> List.length
                                in
                                if charCount >= low && charCount <= high then
                                    Just (Password text)

                                else
                                    Nothing
                            )
                        |> List.length
                        |> String.fromInt
                        |> (++) "Answer: "
                        |> response

                Err deadEnds ->
                    response (Debug.toString deadEnds)
            )

        PartTwo ->
            ( model
            , case Parser.run parsePossiblePasswords model.input of
                Ok possibelPasswords ->
                    possibelPasswords
                        |> List.filterMap
                            (\{ low, high, character, text } ->
                                let
                                    charIndicies =
                                        String.indices (String.fromChar character) text

                                    hasLow =
                                        List.member (low - 1) charIndicies

                                    hasHigh =
                                        List.member (high - 1) charIndicies
                                in
                                if xor hasLow hasHigh then
                                    Just (Password text)

                                else
                                    Nothing
                            )
                        |> List.length
                        |> String.fromInt
                        |> (++) "Answer: "
                        |> response

                Err deadEnds ->
                    response (Debug.toString deadEnds)
            )


type Password
    = Password String


type alias PossiblePassword =
    { low : Int
    , high : Int
    , character : Char
    , text : String
    }


parsePossiblePasswords : Parser (List PossiblePassword)
parsePossiblePasswords =
    Parser.loop [] parsePossiblePasswordHelper


parsePossiblePasswordHelper : List PossiblePassword -> Parser (Step (List PossiblePassword) (List PossiblePassword))
parsePossiblePasswordHelper passwords =
    Parser.oneOf
        [ Parser.succeed (\password -> Loop (password :: passwords))
            |= parsePossiblePassword
            |. Parser.spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Done passwords)
        ]


parsePossiblePassword : Parser PossiblePassword
parsePossiblePassword =
    Parser.succeed PossiblePassword
        |= Parser.int
        |. Parser.symbol "-"
        |= Parser.int
        |. Parser.spaces
        |= parseChar
        |. Parser.symbol ":"
        |. Parser.spaces
        |= (Parser.succeed ()
                |. Parser.chompWhile Char.isAlpha
                |> Parser.getChompedString
           )


parseChar : Parser Char
parseChar =
    Parser.succeed ()
        |. Parser.chompIf Char.isAlpha
        |> Parser.getChompedString
        |> Parser.andThen
            (\str ->
                case String.toList str of
                    [ c ] ->
                        Parser.succeed c

                    _ ->
                        Parser.problem ("Expected a single char but got: " ++ str)
            )
