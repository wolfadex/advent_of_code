module Day03 exposing (run)

import Cli
import Grid exposing (Grid)
import Pages.Script exposing (Script)


run : Script
run =
    Cli.run
        (\input ->
            { part1 =
                input
                    |> String.lines
                    |> toGrid
                    |> findPartNums
                    |> List.sum
                    |> String.fromInt
            , part2 =
                "CARL"
            }
        )



-- PART 1


toGrid : List String -> Grid Char
toGrid lines =
    List.foldl
        (\line ( grid, y ) ->
            ( String.foldl
                (\char ( grid_, x ) ->
                    ( Grid.insert { x = x, y = y } char grid_
                    , x + 1
                    )
                )
                ( grid, 0 )
                line
                |> Tuple.first
            , y + 1
            )
        )
        ( Grid.empty, 0 )
        lines
        |> Tuple.first


findPartNums : Grid Char -> List Int
findPartNums grid =
    findPartNumsHelper { x = 0, y = 0 } grid []


findPartNumsHelper : Grid.Position -> Grid Char -> List Int -> List Int
findPartNumsHelper currentPos grid acc =
    case Grid.get currentPos grid of
        Nothing ->
            acc

        Just char ->
            if Char.isDigit char then
                let
                    neighbors =
                        Grid.neighborList currentPos grid
                in
                if List.any isSymbol neighbors then
                    let
                        ( nextPos, num ) =
                            gatherDigits (getDigitsStart currentPos grid) grid []
                                |> Tuple.mapSecond (String.toInt >> Maybe.withDefault 0)
                    in
                    findPartNumsHelper nextPos grid (num :: acc)

                else
                    case Grid.next currentPos grid of
                        Nothing ->
                            acc

                        Just ( nextPos, _ ) ->
                            findPartNumsHelper nextPos grid acc

            else
                case Grid.next currentPos grid of
                    Nothing ->
                        acc

                    Just ( nextPos, _ ) ->
                        findPartNumsHelper nextPos grid acc


getDigitsStart : Grid.Position -> Grid Char -> Grid.Position
getDigitsStart currentPos grid =
    case (Grid.neighbors currentPos grid).left of
        Nothing ->
            currentPos

        Just char ->
            if Char.isDigit char then
                getDigitsStart { currentPos | x = currentPos.x - 1 } grid

            else
                currentPos


gatherDigits : Grid.Position -> Grid Char -> List Char -> ( Grid.Position, String )
gatherDigits currentPos grid acc =
    case Grid.get currentPos grid of
        Nothing ->
            ( currentPos
            , acc
                |> List.reverse
                |> String.fromList
            )

        Just char ->
            if Char.isDigit char then
                case (Grid.neighbors currentPos grid).right of
                    Nothing ->
                        ( currentPos
                        , acc
                            |> List.reverse
                            |> String.fromList
                        )

                    Just _ ->
                        gatherDigits { currentPos | x = currentPos.x + 1 } grid (char :: acc)

            else
                ( currentPos
                , acc
                    |> List.reverse
                    |> String.fromList
                )



-- findPartNums : Grid Char -> List Int
-- findPartNums grid =
--     Grid.indexedFoldl
--         (\pos char ( nums, partNumFound ) ->
--             if partNumFound then
--                 if Char.isDigit char then
--                     ( nums, partNumFound )
--                 else
--                     ( nums, False )
--             else if Char.isDigit char then
--                 let
--                     neighbors =
--                         Grid.neighborList pos grid
--                 in
--                 if List.any isSymbol neighbors then
--                     ( (grid
--                         |> gatherToTheRight (toLeftMost pos grid)
--                         |> String.toInt
--                         |> Maybe.withDefault 0
--                       )
--                         :: nums
--                     , True
--                     )
--                 else
--                     ( nums, partNumFound )
--             else
--                 ( nums, partNumFound )
--         )
--         ( [], False )
--         grid
--         |> Tuple.first


isSymbol : Char -> Bool
isSymbol char =
    not (Char.isDigit char) && char /= '.'


toLeftMost : Grid.Position -> Grid Char -> Grid.Position
toLeftMost currentPos grid =
    case (Grid.neighbors currentPos grid).left of
        Nothing ->
            currentPos

        Just char ->
            if Char.isDigit char then
                toLeftMost { currentPos | x = currentPos.x - 1 } grid

            else
                currentPos


gatherToTheRight : Grid.Position -> Grid Char -> String
gatherToTheRight currentPos grid =
    gatherToTheRightHelper currentPos grid []
        |> List.reverse
        |> String.fromList


gatherToTheRightHelper : Grid.Position -> Grid Char -> List Char -> List Char
gatherToTheRightHelper currentPos grid chars =
    case Grid.get currentPos grid of
        Nothing ->
            chars

        Just char ->
            if Char.isDigit char then
                gatherToTheRightHelper { currentPos | x = currentPos.x + 1 } grid (char :: chars)

            else
                chars



-- PART 2
-- COMMON
