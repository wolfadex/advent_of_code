module Day01 exposing (run)

import Cli
import Pages.Script exposing (Script)
import Regex exposing (Regex)


run : Script
run =
    Cli.run
        (\input ->
            { part1 =
                input
                    |> String.lines
                    |> List.map firstLastDigitsToNum
                    |> List.sum
                    |> String.fromInt
            , part2 =
                input
                    |> String.lines
                    |> List.map firstLastNumbersToNum
                    |> List.sum
                    |> String.fromInt
            }
        )



-- PART 1


firstLastDigitsToNum : String -> Int
firstLastDigitsToNum str =
    str
        |> String.filter Char.isDigit
        |> (\str_ -> String.left 1 str_ ++ String.right 1 str_)
        |> String.toInt
        |> Maybe.withDefault 0



-- PART 2


firstLastNumbersToNum : String -> Int
firstLastNumbersToNum str =
    let
        matches : List ( Int, String )
        matches =
            numberWords
                |> List.concatMap
                    (\( wordRegex, num ) ->
                        str
                            |> Regex.find wordRegex
                            |> List.map (\match -> ( match.index, num ))
                    )
                |> List.sortBy Tuple.first

        first : String
        first =
            matches
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault "0"

        last : String
        last =
            matches
                |> List.reverse
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault "0"
    in
    (first ++ last)
        |> String.toInt
        |> Maybe.withDefault 0


numberWords : List ( Regex, String )
numberWords =
    List.map (Tuple.mapFirst alwaysRegex)
        [ ( "zero", "0" )
        , ( "one", "1" )
        , ( "two", "2" )
        , ( "three", "3" )
        , ( "four", "4" )
        , ( "five", "5" )
        , ( "six", "6" )
        , ( "seven", "7" )
        , ( "eight", "8" )
        , ( "nine", "9" )
        , ( "0", "0" )
        , ( "1", "1" )
        , ( "2", "2" )
        , ( "3", "3" )
        , ( "4", "4" )
        , ( "5", "5" )
        , ( "6", "6" )
        , ( "7", "7" )
        , ( "8", "8" )
        , ( "9", "9" )
        ]


alwaysRegex : String -> Regex
alwaysRegex str =
    Regex.fromString str
        |> Maybe.withDefault Regex.never
