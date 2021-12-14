module Day04 exposing (program)

import IO
import List.Extra
import Parser exposing ((|.), (|=), Parser, Step(..))
import Posix.IO exposing (IO, Process)


program : Process -> IO ()
program =
    IO.load
        { part1 =
            makeGame
                >> playBingo
                >> Tuple.mapFirst
                    (List.filterMap
                        (\cell ->
                            case cell of
                                Unmarked i ->
                                    Just i

                                Marked _ ->
                                    Nothing
                        )
                        >> List.sum
                    )
                >> (\( winningSum, winningNum ) -> winningSum * winningNum)
                >> String.fromInt
        , part2 =
            makeGame
                >> playReverseBingo
                >> Tuple.mapFirst
                    (List.filterMap
                        (\cell ->
                            case cell of
                                Unmarked i ->
                                    Just i

                                Marked _ ->
                                    Nothing
                        )
                        >> List.sum
                    )
                >> (\( losingSum, winningNum ) -> losingSum * winningNum)
                >> String.fromInt
        }


playBingo : Game -> ( Board, Int )
playBingo game =
    case game.nums of
        [] ->
            ( [], -1 )

        nextNum :: restNums ->
            let
                nextBoards =
                    updateBoards nextNum game.boards
            in
            case findWinner nextBoards of
                Just winner ->
                    ( winner, nextNum )

                Nothing ->
                    playBingo { nums = restNums, boards = nextBoards }


updateBoards : Int -> List Board -> List Board
updateBoards nextNum boards =
    List.map (updateBoard nextNum) boards


updateBoard : Int -> (List Cell -> List Cell)
updateBoard nextNum =
    List.map
        (\cell ->
            case cell of
                Unmarked i ->
                    if nextNum == i then
                        Marked i

                    else
                        cell

                Marked _ ->
                    cell
        )


playReverseBingo : Game -> ( Board, Int )
playReverseBingo game =
    case game.nums of
        [] ->
            ( [], -1 )

        nextNum :: restNums ->
            let
                nextBoards =
                    updateBoards nextNum game.boards

                remainingLosers =
                    List.filter (not << isWinner) nextBoards
            in
            case remainingLosers of
                [] ->
                    ( List.filter (not << isWinner) game.boards
                        |> List.head
                        |> Maybe.map (updateBoard nextNum)
                        |> Maybe.withDefault []
                    , nextNum
                    )

                _ ->
                    playReverseBingo { nums = restNums, boards = nextBoards }


findWinner : List (List Cell) -> Maybe Board
findWinner boards =
    case boards of
        [] ->
            Nothing

        nextBoard :: restBoards ->
            if isWinner nextBoard then
                Just nextBoard

            else
                findWinner restBoards


isWinner : List Cell -> Bool
isWinner board =
    let
        markedCells =
            List.Extra.findIndices isMarked board
    in
    List.any (List.all (\i -> List.member i markedCells))
        [ -- Winning rows
          [ 0, 1, 2, 3, 4 ]
        , [ 5, 6, 7, 8, 9 ]
        , [ 10, 11, 12, 13, 14 ]
        , [ 15, 16, 17, 18, 19 ]
        , [ 20, 21, 22, 23, 24 ]

        -- Winning columns
        , [ 0, 5, 10, 15, 20 ]
        , [ 1, 6, 11, 16, 21 ]
        , [ 2, 7, 12, 17, 22 ]
        , [ 3, 8, 13, 18, 23 ]
        , [ 4, 9, 14, 19, 24 ]
        ]


isMarked : Cell -> Bool
isMarked cell =
    case cell of
        Marked _ ->
            True

        Unmarked _ ->
            False


makeGame : String -> Game
makeGame input =
    case String.split "\n\n" input of
        [] ->
            { nums = [], boards = [] }

        numsStr :: boardsStr ->
            { nums = List.filterMap String.toInt (String.split "," numsStr)
            , boards = List.filterMap (Parser.run parseBoard >> Result.toMaybe) boardsStr
            }


parseBoard : Parser Board
parseBoard =
    Parser.succeed identity
        |. Parser.spaces
        |= Parser.loop [] parseBoardHelper
        |. Parser.end


parseBoardHelper : List Cell -> Parser (Step (List Cell) (List Cell))
parseBoardHelper reverseCells =
    Parser.oneOf
        [ Parser.succeed (\i -> Loop (Unmarked i :: reverseCells))
            |= Parser.int
            |. Parser.spaces
        , Parser.succeed (Done (List.reverse reverseCells))
        ]


type alias Game =
    { nums : List Int
    , boards : List Board
    }


type alias Board =
    List Cell


type Cell
    = Unmarked Int
    | Marked Int
