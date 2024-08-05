module Day05 exposing (run)

import Cli
import Dict exposing (Dict)
import List.Extra
import Pages.Script exposing (Script)
import Regex


run : Script
run =
    Cli.run
        (\input ->
            { part1 =
                input
                    |> toState
                    |> runUntil "location"
                    |> .seeds
                    |> List.minimum
                    |> Maybe.withDefault 0
                    |> String.fromInt
            , part2 =
                input
                    |> toState2
                    |> runUntil2 "location"
                    |> .seeds
                    |> List.minimum
                    |> Maybe.withDefault 0
                    |> String.fromInt
            }
        )


type alias State =
    { seeds : List Int
    , place : String
    , mappers : Dict String Mapper
    }


type alias Mapper =
    { name : String
    , ranges : List { min : Int, max : Int, difference : Int }
    }


type alias State2 =
    { seeds : List Seed
    , place : String
    , mappers : Dict String Mapper
    }


type alias Seed =
    { start : Int
    , end : Int
    }



-- PART 1


toState : String -> State
toState input =
    case String.split "\n\n" input of
        [] ->
            { seeds = []
            , place = ""
            , mappers = Dict.empty
            }

        seedsStr :: mapperStrs ->
            let
                seeds =
                    seedsStr
                        |> String.replace "seeds:" ""
                        |> String.split " "
                        |> List.filterMap (String.trim >> String.toInt)

                mappers =
                    mapperStrs
                        |> List.filterMap toMapper
            in
            { seeds = seeds
            , place = "seed"
            , mappers = Dict.fromList mappers
            }


runUntil : String -> State -> State
runUntil destination state =
    if destination == state.place then
        state

    else
        case Dict.get state.place state.mappers of
            Nothing ->
                state

            Just mapper ->
                runUntil destination
                    { state
                        | seeds =
                            List.map (applyRange mapper.ranges)
                                state.seeds
                        , place = mapper.name
                    }


applyRange : List { min : Int, max : Int, difference : Int } -> Int -> Int
applyRange ranges input =
    case ranges of
        [] ->
            input

        { min, max, difference } :: rest ->
            if min <= input && input <= max then
                input + difference

            else
                applyRange rest input



-- PART 2


toState2 : String -> State2
toState2 input =
    case String.split "\n\n" input of
        [] ->
            { seeds = []
            , place = ""
            , mappers = Dict.empty
            }

        seedsStr :: mapperStrs ->
            let
                seeds =
                    seedsStr
                        |> String.replace "seeds:" ""
                        |> String.split " "
                        |> List.filterMap (String.trim >> String.toInt)
                        |> List.Extra.groupsOf 2
                        |> List.filterMap
                            (\range ->
                                case range of
                                    [ from, to ] ->
                                        Just { start = from, end = from + to }

                                    _ ->
                                        Nothing
                            )

                mappers =
                    mapperStrs
                        |> List.filterMap toMapper
            in
            { seeds = seeds
            , place = "seed"
            , mappers = Dict.fromList mappers
            }


runUntil2 : String -> State2 -> State2
runUntil2 destination state =
    if destination == state.place then
        state

    else
        case Dict.get state.place state.mappers of
            Nothing ->
                state

            Just mapper ->
                runUntil2 destination
                    { state
                        | seeds =
                            List.concatMap (applyRange2 mapper.ranges [])
                                state.seeds
                        , place = mapper.name
                    }


applyRange2 : List { min : Int, max : Int, difference : Int } -> Seed -> List Seed -> List Seed
applyRange2 ranges input output =
    case ranges of
        [] ->
            output

        { min, max, difference } :: rest ->
            applyRange2
                rest
                input
                (List.concatMap
                    [ if input.end < min then
                        [ { start = input.start, end = min - 1 }
                        ]

                      else if input.start < min then
                        List.concatMap
                            [ [ { start = input.start, end = min - 1 } ]
                            , if input.end > max then
                                [ { start = min, end = max }
                                , { start = max + 1, end = input.end }
                                ]

                              else
                                [ { start = min, end = input.end } ]
                            ]

                    else if input.
                      else
                        []
                    , output
                    ]
                )



-- if min <= input && input <= max then
--     input + difference
-- else
--     applyRange rest input
-- COMMON


toMapper : String -> Maybe ( String, Mapper )
toMapper str =
    case String.lines str of
        [] ->
            Nothing

        name :: rangesStrs ->
            case
                name
                    |> String.replace "map:" ""
                    |> String.split "-"
            of
                [ from, _, to ] ->
                    Just
                        ( String.trim from
                        , { name = String.trim to
                          , ranges =
                                rangesStrs
                                    |> List.filterMap
                                        (\str_ ->
                                            case
                                                str_
                                                    |> String.split " "
                                                    |> List.filterMap (String.trim >> String.toInt)
                                            of
                                                [ toNum, fromNum, range ] ->
                                                    Just
                                                        { min = fromNum
                                                        , max = fromNum + range
                                                        , difference = toNum - fromNum
                                                        }

                                                _ ->
                                                    Nothing
                                        )
                          }
                        )

                _ ->
                    Nothing
