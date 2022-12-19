module Day01 exposing (program)

import IO
import Posix.IO exposing (IO, Process)


program : Process -> IO ()
program =
    IO.load
        { part1 =
            \input ->
                input
                    |> String.split "\n\n"
                    |> List.map (String.split "\n" >> List.map (String.toInt >> Maybe.withDefault 0) >> List.sum)
                    |> List.maximum
                    |> Maybe.withDefault 0
                    |> String.fromInt
        , part2 =
            \input ->
                input
                    |> String.split "\n\n"
                    |> List.map (String.split "\n" >> List.map (String.toInt >> Maybe.withDefault 0) >> List.sum)
                    |> List.sort
                    |> List.reverse
                    |> List.take 3
                    |> List.sum
                    |> String.fromInt
        }
