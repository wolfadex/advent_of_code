port module Day03 exposing (main)

import Array exposing (Array)
import Platform
import Point exposing (Point)


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
                |> buildData
                |> rideTheToboggan (Point.new { x = 3, y = 1 }) Point.zero 0
                |> String.fromInt
                |> (++) "Answer: "
                |> response
            )

        PartTwo ->
            ( model
            , let
                baseData =
                    buildData model.input

                route11 =
                    rideTheToboggan (Point.new { x = 1, y = 1 }) Point.zero 0 baseData

                route31 =
                    rideTheToboggan (Point.new { x = 3, y = 1 }) Point.zero 0 baseData

                route51 =
                    rideTheToboggan (Point.new { x = 5, y = 1 }) Point.zero 0 baseData

                route71 =
                    rideTheToboggan (Point.new { x = 7, y = 1 }) Point.zero 0 baseData

                route12 =
                    rideTheToboggan (Point.new { x = 1, y = 2 }) Point.zero 0 baseData
              in
              (route11 * route31 * route51 * route71 * route12)
                |> String.fromInt
                |> (++) "Answer: "
                |> response
            )


buildData : String -> { width : Int, height : Int, grid : Array Char }
buildData gridStr =
    let
        rows =
            String.split "\n" gridStr

        width =
            rows
                |> List.head
                |> Maybe.withDefault ""
                |> String.length

        height =
            List.length rows

        grid =
            rows
                |> String.join ""
                |> String.toList
                |> Array.fromList
    in
    { width = width, height = height, grid = grid }


rideTheToboggan : Point -> Point -> Int -> { width : Int, height : Int, grid : Array Char } -> Int
rideTheToboggan moveDir pos treesHit ({ width, height, grid } as data) =
    let
        newPos =
            Point.add pos moveDir

        adjustedPos =
            if Point.getX newPos >= width then
                Point.add (Point.new { y = 0, x = -width }) newPos

            else
                newPos
    in
    if Point.getY adjustedPos >= height then
        treesHit

    else
        let
            newTreesHit =
                if isTree grid width adjustedPos then
                    treesHit + 1

                else
                    treesHit
        in
        rideTheToboggan moveDir adjustedPos newTreesHit data


isTree : Array Char -> Int -> Point -> Bool
isTree grid width pos =
    let
        x =
            Point.getX pos

        y =
            Point.getY pos
    in
    case Array.get (y * width + x) grid of
        Just '#' ->
            True

        _ ->
            False
