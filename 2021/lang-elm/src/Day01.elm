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
                "carl"
        }
