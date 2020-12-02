port module Day01 exposing (main)

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
                |> List.filterMap String.toInt
                |> List.partition ((<) 1010)
                |> findNumber
                --|> findNums2 2020
                --|> Maybe.map (\(a, b) -> a * b)
                --|> Maybe.withDefault -1
                |> String.fromInt
                |> (++) "Answer: "
                |> response
            )

        PartTwo ->
            ( model
            , model.input
                |> String.split "\n"
                |> List.filterMap String.toInt
                |> findNums3
                |> String.fromInt
                |> (++) "Answer: "
                |> response
            )


findNumber : ( List Int, List Int ) -> Int
findNumber ( lows, highs ) =
    case lows of
        [] ->
            if List.member 1010 highs then
                1010 ^ 2

            else
                -1

        a :: rest ->
            if List.member (2020 - a) highs then
                a * (2020 - a)

            else
                findNumber ( rest, highs )


findNums3 : List Int -> Int
findNums3 ints =
    case ints of
        [] ->
            -1

        a :: rest ->
            case findNums2 (2020 - a) rest of
                Just ( b, c ) ->
                    a * b * c

                Nothing ->
                    findNums3 rest


findNums2 : Int -> List Int -> Maybe ( Int, Int )
findNums2 goal ints =
    case ints of
        [] ->
            Nothing

        a :: rest ->
            if List.member (goal - a) rest then
                Just ( a, goal - a )

            else
                findNums2 goal rest
