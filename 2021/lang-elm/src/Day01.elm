module Day01 exposing (program)

import IO
import Posix.IO exposing (IO, Process)


program : Process -> IO ()
program =
    IO.load
        { part1 =
            \content ->
                case content |> String.lines |> List.filterMap String.toInt of
                    [] ->
                        "0"

                    first :: rest ->
                        List.foldl
                            (\depth ( prevDepth, increases ) ->
                                ( depth
                                , if depth > prevDepth then
                                    increases + 1

                                  else
                                    increases
                                )
                            )
                            ( first, 0 )
                            rest
                            |> Tuple.second
                            |> String.fromInt
        , part2 =
            \content ->
                content
                    |> String.lines
                    |> List.filterMap String.toInt
                    |> slidingWindow
        }


slidingWindow : List Int -> String
slidingWindow depths =
    case depths of
        a :: b :: c :: rest ->
            slidingWindowHelper (a + b + c) (b :: c :: rest) 0

        _ ->
            "-1"


slidingWindowHelper : Int -> List Int -> Int -> String
slidingWindowHelper previousDepth depths increases =
    case depths of
        a :: b :: c :: rest ->
            let
                depth =
                    a + b + c

                newIncreases =
                    if depth > previousDepth then
                        increases + 1

                    else
                        increases
            in
            slidingWindowHelper depth (b :: c :: rest) newIncreases

        _ ->
            String.fromInt increases
