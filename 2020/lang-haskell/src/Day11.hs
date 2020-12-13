{-# LANGUAGE LambdaCase #-}

module Day11 (solve1, solve2) where

import Data.Matrix (Matrix)
import Data.Matrix as Matrix
import Data.Maybe as Maybe

solve1 :: String -> Int
solve1 = occupiedCount . musicalChairs nearbySeats 4 . Matrix.mapPos parseToSpace . Matrix.fromLists . lines -- inputToMap

solve2 :: String -> Int
solve2 = occupiedCount . musicalChairs firstSeatInSight 5 . Matrix.mapPos parseToSpace . Matrix.fromLists . lines -- inputToMap

data Space
  = Floor
  | SeatEmpty
  | SeatOccupied
  deriving (Eq)

instance Show Space where
  show s = case s of
    Floor -> "."
    SeatEmpty -> "L"
    SeatOccupied -> "#"

type NearByFn = Matrix Space -> (Int, Int) -> [Space]

parseToSpace :: (Int, Int) -> Char -> Space
parseToSpace _ = \case
  '.' -> Floor
  'L' -> SeatEmpty
  '#' -> SeatOccupied
  _ -> Floor

musicalChairs :: NearByFn -> Int -> Matrix Space -> Matrix Space
musicalChairs nearByFn minSisterSeats previousSeating =
  if previousSeating == nextSeating
    then previousSeating
    else musicalChairs nearByFn minSisterSeats nextSeating
  where
    nextSeating = updateSeats nearByFn minSisterSeats previousSeating

updateSeats :: NearByFn -> Int -> Matrix Space -> Matrix Space
updateSeats nearByFn minSisterSeats grid =
 Matrix.mapPos
    (\(x, y) space ->
      case space of
        SeatEmpty ->
          if SeatOccupied `elem` getNearbySeats (x, y)
            then space
            else SeatOccupied
        SeatOccupied ->
          if (>= minSisterSeats) $ length $ justOccupiedSeats $ getNearbySeats (x, y)
            then SeatEmpty
            else space
        Floor -> Floor
    )
    grid
  where
    getNearbySeats = nearByFn grid
    justOccupiedSeats = Prelude.foldr (\space acc -> if space == SeatOccupied then space : acc else acc) []

nearbySeats :: NearByFn
nearbySeats grid (x, y) =
  Maybe.catMaybes [ Matrix.safeGet (x - 1) (y - 1) grid
    , Matrix.safeGet (x - 1) y grid
    , Matrix.safeGet (x - 1) (y + 1) grid
    , Matrix.safeGet x (y - 1) grid
    , Matrix.safeGet x (y + 1) grid
    , Matrix.safeGet (x + 1) (y - 1) grid
    , Matrix.safeGet (x + 1) y grid
    , Matrix.safeGet (x + 1) (y + 1) grid
    ]

firstSeatInSight :: NearByFn
firstSeatInSight grid (x, y) =
  Maybe.catMaybes [ firstSeatOrNothing grid (x, y) (-1, -1)
    , firstSeatOrNothing grid (x, y) (-1, 0)
    , firstSeatOrNothing grid (x, y) (-1, 1)
    , firstSeatOrNothing grid (x, y) (0, -1)
    , firstSeatOrNothing grid (x, y) (0, 1)
    , firstSeatOrNothing grid (x, y) (1, -1)
    , firstSeatOrNothing grid (x, y) (1, 0)
    , firstSeatOrNothing grid (x, y) (1, 1)
    ]

firstSeatOrNothing :: Matrix Space -> (Int, Int) -> (Int, Int) -> Maybe Space
firstSeatOrNothing grid (x1, y1) dir@(x2, y2) =
  case Matrix.safeGet nextX nextY grid of
    Nothing -> Nothing
    Just s ->
      case s of
        SeatOccupied -> Just s
        SeatEmpty -> Just s
        Floor -> firstSeatOrNothing grid (nextX, nextY) dir
  where
    nextX = x1 + x2
    nextY = y1 + y2

occupiedCount :: Matrix Space -> Int
occupiedCount =
  Prelude.foldl
    (\total space -> if space == SeatOccupied then total + 1 else total)
    0
    . Matrix.toList