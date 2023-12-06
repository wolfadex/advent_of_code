module Grid exposing (..)

import Dict exposing (Dict)


type Grid a
    = Grid (Dict ( Int, Int ) a)


type alias Position =
    { x : Int
    , y : Int
    }


empty : Grid a
empty =
    Grid Dict.empty


insert : Position -> a -> Grid a -> Grid a
insert { x, y } value (Grid grid) =
    Grid (Dict.insert ( y, x ) value grid)


get : Position -> Grid a -> Maybe a
get { x, y } (Grid grid) =
    Dict.get ( y, x ) grid


previous : Position -> Grid a -> Maybe ( Position, a )
previous { x, y } (Grid grid) =
    if x < 1 then
        grid
            |> Dict.filter (\( y_, _ ) _ -> y_ == y - 1)
            |> Dict.toList
            |> List.sortBy (\( ( _, x_ ), _ ) -> x_)
            |> List.reverse
            |> List.head
            |> Maybe.map (Tuple.mapFirst (\( y_, x_ ) -> { x = x_, y = y_ }))

    else
        grid
            |> Dict.get ( y, x - 1 )
            |> Maybe.map (\value -> ( { x = x - 1, y = y }, value ))


next : Position -> Grid a -> Maybe ( Position, a )
next { x, y } (Grid grid) =
    case Dict.get ( y, x + 1 ) grid of
        Nothing ->
            grid
                |> Dict.get ( y + 1, 0 )
                |> Maybe.map (\value -> ( { x = 0, y = y + 1 }, value ))

        Just value ->
            Just ( { x = x + 1, y = y }, value )


neighbors :
    Position
    -> Grid a
    ->
        { above : Maybe a
        , aboveRight : Maybe a
        , right : Maybe a
        , belowRight : Maybe a
        , below : Maybe a
        , belowLeft : Maybe a
        , left : Maybe a
        , aboveLeft : Maybe a
        }
neighbors { x, y } grid =
    { above = get { x = x, y = y - 1 } grid
    , aboveRight = get { x = x + 1, y = y - 1 } grid
    , right = get { x = x + 1, y = y } grid
    , belowRight = get { x = x + 1, y = y + 1 } grid
    , below = get { x = x, y = y + 1 } grid
    , belowLeft = get { x = x - 1, y = y + 1 } grid
    , left = get { x = x - 1, y = y } grid
    , aboveLeft = get { x = x - 1, y = y - 1 } grid
    }


neighborList : Position -> Grid a -> List a
neighborList pos grid =
    let
        neigh =
            neighbors pos grid
    in
    List.filterMap identity
        [ neigh.above
        , neigh.aboveRight
        , neigh.right
        , neigh.belowRight
        , neigh.below
        , neigh.belowLeft
        , neigh.left
        , neigh.aboveLeft
        ]


indexedFoldl : (Position -> a -> b -> b) -> b -> Grid a -> b
indexedFoldl f acc (Grid grid) =
    Dict.foldl
        (\( y, x ) -> f { x = x, y = y })
        acc
        grid
