port module Day06 exposing (main)

import Platform
import Set exposing (Set)


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
                |> String.split "\n\n"
                |> List.map (String.split "" >> Set.fromList >> Set.filter ((/=) "\n") >> Set.size)
                |> List.sum
                |> String.fromInt
                |> (++) "Answer: "
                |> response
            )

        PartTwo ->
            ( model
            , model.input
                |> String.split "\n\n"
                |> List.map tallyGroupYes
                |> List.sum
                |> String.fromInt
                |> (++) "Answer: "
                |> response
            )


tallyGroupYes : String -> Int
tallyGroupYes =
    String.split "\n"
        >> List.map (String.split "" >> Set.fromList)
        >> (\persons ->
                let
                    firstPerson =
                        List.head persons
                            |> Maybe.withDefault Set.empty
                in
                List.foldl Set.intersect firstPerson persons
           )
        >> Set.size
