module Day02 exposing (program)

import IO
import Parser exposing ((|.), (|=), Parser, Step(..))
import Posix.IO exposing (IO, Process)


program : Process -> IO ()
program =
    IO.load
        { part1 =
            \input ->
                case Parser.run parseCommands input of
                    Err _ ->
                        "Error"

                    Ok commands ->
                        navigate1 commands ( 0, 0 )
                            |> String.fromInt
        , part2 =
            \input ->
                case Parser.run parseCommands input of
                    Err _ ->
                        "Error"

                    Ok commands ->
                        navigate2 commands ( 0, 0, 0 )
                            |> String.fromInt
        }


navigate2 : List Command -> (Int, Int, Int) -> Int
navigate2 commands ( depth, x, aim ) =
    case commands of
        [] ->
            depth * x

        (Forward n) :: rest ->
            navigate2 rest ( depth + n * aim, x + n, aim )

        (Up n) :: rest ->
            navigate2 rest ( depth, x, aim - n )

        (Down n) :: rest ->
            navigate2 rest ( depth, x, aim + n )


navigate1 : List Command -> ( Int, Int ) -> Int
navigate1 commands ( depth, x ) =
    case commands of
        [] ->
            depth * x

        (Forward n) :: rest ->
            navigate1 rest ( depth, x + n )

        (Up n) :: rest ->
            navigate1 rest ( depth - n, x )

        (Down n) :: rest ->
            navigate1 rest ( depth + n, x )


type Command
    = Forward Int
    | Up Int
    | Down Int


parseCommands : Parser (List Command)
parseCommands =
    Parser.loop [] parseCommandsHelper


parseCommandsHelper : List Command -> Parser (Step (List Command) (List Command))
parseCommandsHelper reverseCommands =
    Parser.oneOf
        [ Parser.succeed (\command -> Loop (command :: reverseCommands))
            |= parseCommand
        , Parser.succeed (Done (List.reverse reverseCommands))
        ]


parseCommand : Parser Command
parseCommand =
    Parser.oneOf
        [ parseForward
        , parseUp
        , parseDown
        ]


parseForward : Parser Command
parseForward =
    Parser.succeed Forward
        |. Parser.keyword "forward"
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces


parseUp : Parser Command
parseUp =
    Parser.succeed Up
        |. Parser.keyword "up"
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces


parseDown : Parser Command
parseDown =
    Parser.succeed Down
        |. Parser.keyword "down"
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
