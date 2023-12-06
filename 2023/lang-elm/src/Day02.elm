module Day02 exposing (run)

import Cli
import Pages.Script exposing (Script)


run : Script
run =
    Cli.run
        (\input ->
            { part1 =
                input
                    |> String.lines
                    |> List.filterMap parseGame
                    |> List.filter gameIsValid
                    |> List.map .id
                    |> List.sum
                    |> String.fromInt
            , part2 =
                input
                    |> String.lines
                    |> List.filterMap parseGame
                    |> List.map toPower
                    |> List.sum
                    |> String.fromInt
            }
        )


type alias Game =
    { id : Int
    , sets : List { red : Int, green : Int, blue : Int }
    }



-- PART 1


maxRed : Int
maxRed =
    12


maxGreen : Int
maxGreen =
    13


maxBlue : Int
maxBlue =
    14


gameIsValid : Game -> Bool
gameIsValid game =
    List.all
        (\set ->
            (set.red <= maxRed)
                && (set.green <= maxGreen)
                && (set.blue <= maxBlue)
        )
        game.sets



-- PART 2


toPower : Game -> Int
toPower game =
    -- let
    --     _ =
    --         Debug.log "game" game
    -- in
    game.sets
        |> List.foldl
            (\set acc ->
                { red = max set.red acc.red
                , green = max set.green acc.green
                , blue = max set.blue acc.blue
                }
            )
            { red = 0
            , green = 0
            , blue = 0
            }
        |> (\{ red, green, blue } -> red * green * blue)



-- COMMON


parseGame : String -> Maybe Game
parseGame str =
    case String.split ":" str of
        [ idStr, sets ] ->
            idStr
                |> String.replace "Game" ""
                |> String.trim
                |> String.toInt
                |> Maybe.map
                    (\id ->
                        { id = id
                        , sets = parseSets sets
                        }
                    )

        _ ->
            Nothing


parseSets : String -> List { red : Int, green : Int, blue : Int }
parseSets str =
    str
        |> String.split ";"
        |> List.map parseSet


parseSet : String -> { red : Int, green : Int, blue : Int }
parseSet str =
    str
        |> String.split ","
        |> List.foldl
            (\numColor acc ->
                let
                    trimmed =
                        String.trim numColor
                in
                if String.endsWith "red" trimmed then
                    trimmed
                        |> String.replace "red" ""
                        |> String.trim
                        |> String.toInt
                        |> Maybe.map (\red -> { acc | red = acc.red + red })
                        |> Maybe.withDefault acc

                else if String.endsWith "green" trimmed then
                    trimmed
                        |> String.replace "green" ""
                        |> String.trim
                        |> String.toInt
                        |> Maybe.map (\green -> { acc | green = acc.green + green })
                        |> Maybe.withDefault acc

                else if String.endsWith "blue" trimmed then
                    trimmed
                        |> String.replace "blue" ""
                        |> String.trim
                        |> String.toInt
                        |> Maybe.map (\blue -> { acc | blue = acc.blue + blue })
                        |> Maybe.withDefault acc

                else
                    acc
            )
            { red = 0, green = 0, blue = 0 }
