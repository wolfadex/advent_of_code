port module Day05 exposing (main)

import Html exposing (a)
import Platform


main : Program String Model Msg
main =
    Platform.worker
        { init = init
        , subscriptions = subscriptions
        , update = update
        }


type alias Model =
    { input : String
    }


init : String -> ( Model, Cmd Msg )
init input =
    ( { input = input }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ part1 (\_ -> PartOne)
        , part2 (\_ -> PartTwo)
        ]


port part1 : (String -> msg) -> Sub msg


port part2 : (String -> msg) -> Sub msg


port response : String -> Cmd msg


type Msg
    = PartOne
    | PartTwo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PartOne ->
            ( model
            , model.input
                |> String.split "\n"
                |> List.map findSeat
                |> List.maximum
                |> Maybe.withDefault -1
                |> String.fromInt
                |> (++) "Answer: "
                |> response
            )

        PartTwo ->
            ( model
            , model.input
                |> String.split "\n"
                |> List.map findSeat
                |> List.sort
                |> findGap
                |> String.fromInt
                |> (++) "Answer: "
                |> response
            )


findSeat : String -> Int
findSeat instructions =
    let
        rowInstructions =
            String.left 7 instructions

        row =
            String.foldl
                (\char acc ->
                    case char of
                        'F' ->
                            List.take (List.length acc // 2) acc

                        'B' ->
                            List.drop (List.length acc // 2) acc

                        _ ->
                            acc
                )
                (List.range 0 127)
                rowInstructions
                |> List.head
                |> Maybe.withDefault 0

        columnInstructions =
            String.right 3 instructions

        column =
            String.foldl
                (\char acc ->
                    case char of
                        'L' ->
                            List.take (List.length acc // 2) acc

                        'R' ->
                            List.drop (List.length acc // 2) acc

                        _ ->
                            acc
                )
                (List.range 0 7)
                columnInstructions
                |> List.head
                |> Maybe.withDefault 0
    in
    row * 8 + column


findGap : List Int -> Int
findGap seats =
    case seats of
        a :: b :: rest ->
            if b - a == 2 then
                b - 1

            else
                findGap (b :: rest)

        _ ->
            -1
