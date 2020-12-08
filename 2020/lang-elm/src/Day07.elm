port module Day07 exposing (main)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..))
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
                |> parseInput
                |> (\bags -> calculateParents bags "shiny gold")
                |> Set.size
                |> String.fromInt
                |> (++) "Answer: "
                |> response
            )

        PartTwo ->
            ( model
            , model.input
                |> parseInput
                |> calculateContains "shiny gold"
                |> String.fromInt
                |> (++) "Answer: "
                |> response
            )


calculateContains : ColorId -> Dict ColorId Bag -> Int
calculateContains parentId bags =
    let
        children =
            Dict.get parentId bags
                |> Maybe.map .contains
                |> Maybe.withDefault []
    in
    List.foldl
        (\( count, id ) total ->
            total + count + count * calculateContains id bags
        )
        0
        children


parentBags : Dict ColorId Bag -> ColorId -> Set ColorId
parentBags bags childId =
    Dict.foldl
        (\_ bag acc ->
            let
                containsChild =
                    bag.contains
                        |> List.filter (\( _, id ) -> id == childId)
                        |> List.length
                        |> (\l -> l > 0)
            in
            if containsChild then
                Set.insert bag.color acc

            else
                acc
        )
        Set.empty
        bags


calculateParents : Dict ColorId Bag -> ColorId -> Set ColorId
calculateParents bags childId =
    let
        parents =
            parentBags bags childId
    in
    List.foldl (\parent t -> Set.union (calculateParents bags parent) t) parents (Set.toList parents)


type alias Bag =
    { color : ColorId
    , contains : List ( Int, ColorId )
    }


type alias ColorId =
    String


parseInput : String -> Dict ColorId Bag
parseInput =
    Parser.run parseBags
        >> Result.withDefault Dict.empty


parseBags : Parser (Dict ColorId Bag)
parseBags =
    Parser.loop Dict.empty parseBagsHelper


parseBagsHelper : Dict ColorId Bag -> Parser (Step (Dict ColorId Bag) (Dict ColorId Bag))
parseBagsHelper bags =
    Parser.oneOf
        [ Parser.succeed
            (\bag ->
                Loop (Dict.insert bag.color bag bags)
            )
            |= parseBag
            |. Parser.oneOf
                [ Parser.symbol "\n"
                , Parser.end
                ]
        , Parser.succeed ()
            |> Parser.map (\_ -> Done bags)
        ]


parseBag : Parser Bag
parseBag =
    Parser.succeed Bag
        |= parseColor
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.keyword "contain no other bags." |> Parser.map (\_ -> [])
            , Parser.sequence
                { start = "contain"
                , end = "."
                , spaces = Parser.spaces
                , item = parseChild
                , separator = ","
                , trailing = Forbidden
                }
            ]


parseChild : Parser ( Int, ColorId )
parseChild =
    Parser.succeed Tuple.pair
        |= Parser.int
        |. Parser.spaces
        |= parseColor


parseColor : Parser ColorId
parseColor =
    Parser.succeed (\first second -> first ++ " " ++ second)
        |= (Parser.succeed ()
                |. Parser.chompWhile Char.isAlpha
                |> Parser.getChompedString
           )
        |. Parser.spaces
        |= (Parser.succeed ()
                |. Parser.chompWhile Char.isAlpha
                |> Parser.getChompedString
           )
        |. Parser.spaces
        |. Parser.oneOf
            [ Parser.keyword "bags" |> Parser.backtrackable
            , Parser.keyword "bag"
            ]
