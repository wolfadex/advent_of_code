module Cli exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.File
import BackendTask.Http
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Json.Decode as Decode
import Pages.Script as Script exposing (Script)


run : (String -> { part1 : String, part2 : String }) -> Script
run solver =
    Script.withCliOptions program
        (\{ inputFile } ->
            BackendTask.File.rawFile inputFile
                |> BackendTask.allowFatal
                |> BackendTask.andThen
                    (\input ->
                        let
                            solutions =
                                solver input
                        in
                        Script.log ("Part 1: " ++ solutions.part1 ++ "\nPart 2: " ++ solutions.part2)
                    )
        )


type alias CliOptions =
    { inputFile : String
    }


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.requiredPositionalArg "inputFile")
            )
