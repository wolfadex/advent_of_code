module Day03 exposing (program)

import IO
import List.Extra
import Posix.IO exposing (IO, Process)


program : Process -> IO ()
program =
    IO.load
        { part1 =
            String.lines
                >> List.map stringToBinary
                >> List.Extra.transpose
                >> List.map common
                >> diagnostic
                >> String.fromInt
        , part2 =
            String.lines
                >> List.map stringToBinary
                >> lifeSupport
                >> String.fromInt
        }


diagnostic : List Int -> Int
diagnostic gamma =
    binaryToDecimal gamma * binaryToDecimal (binaryFlip gamma)


lifeSupport : List (List Int) -> Int
lifeSupport binaries =
    binaryToDecimal (mostCommon 0 binaries) * binaryToDecimal (leastCommon 0 binaries)


mostCommon : Int -> List (List Int) -> List Int
mostCommon index remainingBinaries =
    let
        indexedBits =
            List.filterMap (List.Extra.getAt index) remainingBinaries

        ( zeros, ones ) =
            List.foldl
                (\i ( z, o ) ->
                    if i == 0 then
                        ( z + 1, o )

                    else
                        ( z, o + 1 )
                )
                ( 0, 0 )
                indexedBits

        keepN =
            if zeros > ones then
                0

            else
                1
    in
    case List.filter (List.Extra.getAt index >> Maybe.withDefault -1 >> (==) keepN) remainingBinaries of
        [ answer ] ->
            answer

        remaining ->
            mostCommon (index + 1) remaining


leastCommon : Int -> List (List Int) -> List Int
leastCommon index remainingBinaries =
    let
        indexedBits =
            List.filterMap (List.Extra.getAt index) remainingBinaries

        ( zeros, ones ) =
            List.foldl
                (\i ( z, o ) ->
                    if i == 0 then
                        ( z + 1, o )

                    else
                        ( z, o + 1 )
                )
                ( 0, 0 )
                indexedBits

        keepN =
            if zeros > ones then
                1

            else
                0
    in
    case List.filter (List.Extra.getAt index >> Maybe.withDefault -1 >> (==) keepN) remainingBinaries of
        [ answer ] ->
            answer

        remaining ->
            leastCommon (index + 1) remaining


binaryFlip : List Int -> List Int
binaryFlip =
    List.map
        (\i ->
            if i == 0 then
                1

            else
                0
        )


binaryToDecimal : List Int -> Int
binaryToDecimal =
    List.reverse >> List.Extra.findIndices ((==) 1) >> List.map (\i -> 2 ^ i) >> List.sum


common : List Int -> Int
common ints =
    let
        ( zeros, ones ) =
            List.foldl
                (\i ( z, o ) ->
                    if i == 0 then
                        ( z + 1, o )

                    else
                        ( z, o + 1 )
                )
                ( 0, 0 )
                ints
    in
    if zeros > ones then
        0

    else
        1


stringToBinary : String -> List Int
stringToBinary =
    String.toList
        >> List.map
            (\char ->
                case char of
                    '1' ->
                        1

                    _ ->
                        0
            )
