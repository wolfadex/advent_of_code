module Day02 exposing (program)

import IO
import Posix.IO exposing (IO, Process)


program : Process -> IO ()
program =
    IO.load
        { part1 =
            \input ->
                input
                    |> String.lines
                    |> List.map (String.words >> toRPSPair >> Maybe.map scorePair >> Maybe.withDefault 0)
                    |> List.sum
                    |> String.fromInt
        , part2 =
            \input ->
                input
                    |> String.lines
                    |> List.map (String.words >> toRPSState >> Maybe.map (findPair >> scorePair) >> Maybe.withDefault 0)
                    |> List.sum
                    |> String.fromInt
        }


findPair : ( RPS, LDW ) -> ( RPS, RPS )
findPair ((left, _ ) as pair) =
    case pair of
        ( Rock, Lose ) -> ( left, Scissors )
        ( Rock, Win ) -> ( left, Paper )
        ( Scissors, Lose ) -> ( left, Paper )
        ( Scissors, Win ) -> ( left, Rock )
        ( Paper, Lose ) -> ( left, Rock )
        ( Paper, Win ) -> ( left, Scissors )
        ( _, Draw ) -> ( left, left )


toRPSState : List String -> Maybe ( RPS, LDW )
toRPSState words =
    case words of
        [ them, state ] ->
            case (abcRPS them, parseState state ) of
                ( Just left, Just right ) -> Just ( left, right )
                _ -> Nothing
        _ ->
            Nothing


parseState : String -> Maybe LDW
parseState str =
    case str of
        "X" -> Just Lose
        "Y" -> Just Draw
        "Z" -> Just Win
        _ -> Nothing


type LDW
    = Lose
    | Draw
    | Win


scorePair : ( RPS, RPS ) -> Int
scorePair (( _, right ) as pair) =
    case pair of
        ( Rock, Rock ) -> 3 + 1
        ( Rock, Paper ) -> 6 + 2
        ( Rock, Scissors ) -> 0 + 3
        ( Paper, Rock ) -> 0 + 1
        ( Paper, Paper ) -> 3 + 2
        ( Paper, Scissors ) -> 6 + 3
        ( Scissors, Rock ) -> 6 + 1
        ( Scissors, Paper ) -> 0 + 2
        ( Scissors, Scissors ) -> 3 + 3


toRPSPair : List String -> Maybe ( RPS, RPS )
toRPSPair words =
    case words of
        [ abc, xyz ] ->
            case (abcRPS abc, xyzRPS xyz ) of
                ( Just left, Just right ) -> Just (left, right)
                _ -> Nothing
                
        _ -> Nothing

abcRPS : String -> Maybe RPS
abcRPS  str =
    case str of
        "A" -> Just Rock
        "B" -> Just Paper
        "C" -> Just Scissors
        _ -> Nothing

xyzRPS : String -> Maybe RPS
xyzRPS  str =
    case str of
        "X" -> Just Rock
        "Y" -> Just Paper
        "Z" -> Just Scissors
        _ -> Nothing


type RPS
    = Rock
    | Paper
    | Scissors