module IO exposing (..)

import Posix.IO exposing (IO, Process)
import Posix.IO.File
import Posix.IO.Process


load : { part1 : String -> String, part2 : String -> String } -> Process -> IO ()
load { part1, part2 } process =
    case process.argv of
        [ _, filename ] ->
            Posix.IO.File.contentsOf filename
                |> Posix.IO.exitOnError identity
                |> Posix.IO.andThen
                    (\content ->
                        Posix.IO.Process.print ("Part 1: " ++ part1 content)
                            |> Posix.IO.andThen
                                (\() ->
                                    Posix.IO.Process.print ("Part 2: " ++ part2 content)
                                )
                    )

        _ ->
            Posix.IO.Process.logErr "Usage: elm-cli <program> file\n"
