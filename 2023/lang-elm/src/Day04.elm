module Day04 exposing (run)

import Array exposing (Array)
import Cli
import Pages.Script exposing (Script)


run : Script
run =
    Cli.run
        (\input ->
            { part1 =
                input
                    |> String.lines
                    |> List.filterMap parseCard
                    |> List.map (countWins >> score)
                    |> List.sum
                    |> String.fromInt
            , part2 =
                input
                    |> String.lines
                    |> List.filterMap parseCard
                    |> List.map (RealCard 0)
                    |> Array.fromList
                    |> collect
                    |> String.fromInt
            }
        )


type alias Card =
    { winners : List Int
    , myNums : List Int
    }



-- PART 1


score : Int -> Int
score wins =
    if wins == 1 then
        1

    else if wins > 1 then
        1 * 2 ^ (wins - 1)

    else
        0



-- PART 2


type RealOrWinCard
    = RealCard Int Card


collect : Array RealOrWinCard -> Int
collect originals =
    collectHelper originals 0
        |> Array.foldl
            (\(RealCard copies card) acc ->
                acc + 1 + copies
            )
            0


collectHelper : Array RealOrWinCard -> Int -> Array RealOrWinCard
collectHelper originals index =
    case Array.get index originals of
        Just (RealCard copies card) ->
            let
                wins =
                    countWins card
            in
            if wins < 1 then
                collectHelper originals (index + 1)

            else
                collectHelper
                    (Array.indexedMap
                        (\idx (RealCard copies_ card_) ->
                            if idx > index && idx <= index + wins then
                                RealCard (copies_ + 1 + 1 * copies) card_

                            else
                                RealCard copies_ card_
                        )
                        originals
                    )
                    (index + 1)

        Nothing ->
            originals



-- COMMON


parseCard : String -> Maybe Card
parseCard str =
    case String.split ":" str of
        [ _, numbersStr ] ->
            case String.split "|" numbersStr of
                [ winnersStr, myNumsStr ] ->
                    Just
                        { winners = toInts winnersStr
                        , myNums = toInts myNumsStr
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


toInts : String -> List Int
toInts str =
    str
        |> String.split " "
        |> List.filterMap (String.trim >> String.toInt)


countWins : Card -> Int
countWins card =
    card.myNums
        |> List.filter (\num -> List.member num card.winners)
        |> List.length
