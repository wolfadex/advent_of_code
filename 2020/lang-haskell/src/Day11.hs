{-# LANGUAGE LambdaCase #-}

module Day11 (solve1, solve2) where

import Data.Map (Map)
import Data.Map.Strict as Map
import Data.Maybe as Maybe
import Debug.Trace

solve1 :: String -> Int
solve1 = occupiedCount . musicalChairs . inputToMap

solve2 :: String -> Int
solve2 = undefined

data Space =
    Floor
  | SeatEmpty
  | SeatOccupied
  deriving (Eq, Show)

inputToMap :: String -> Map (Int, Int) Space
inputToMap = Map.map charToSpace . (\l -> Debug.Trace.traceShow l l) . outer . lines
  where
    charToSpace = \case
                      '.' -> Floor
                      'L' -> SeatEmpty
                      '#' -> SeatOccupied
                      _ -> Floor
    outer = fst . Prelude.foldl (\(grid, row) rowStr-> (inner (grid, row) rowStr, row + 1)) (Map.empty, 0)
    inner (grid, row) = fst . Prelude.foldl (\(g, col) c -> (Map.insert (row, col) c g, col + 1)) (grid, 0)

musicalChairs :: Map (Int, Int) Space -> Map (Int, Int) Space
musicalChairs previousSeating =
  if previousSeating == nextSeating
    then previousSeating
    else musicalChairs nextSeating
  where
    nextSeating = applyFullSeats $ applyEmptySeats previousSeating

applyEmptySeats :: Map (Int, Int) Space -> Map (Int, Int) Space
applyEmptySeats grid =
  Map.mapWithKey
    (\(x, y) space ->
      case space of
        SeatEmpty ->
          if SeatOccupied `elem` nearbySeats (x, y) grid
            then space
            else SeatOccupied
        _ -> space
    )
    grid

applyFullSeats :: Map (Int, Int) Space -> Map (Int, Int) Space
applyFullSeats grid =
  Map.mapWithKey
    (\(x, y) space ->
      case space of
        SeatOccupied ->
          if (4 ==) $ length $ justOccupiedSeats $ nearbySeats (x, y) grid
            then SeatEmpty
            else space
        _ -> space
    )
    grid
  where
    justOccupiedSeats = Prelude.foldr (\space acc -> if space == SeatOccupied then space : acc else acc) []


nearbySeats :: (Int, Int) -> Map (Int, Int) Space -> [Space]
nearbySeats (x, y) grid =
  Maybe.catMaybes [ Map.lookup (x - 1, y - 1) grid
    , Map.lookup (x - 1, y) grid
    , Map.lookup (x - 1, y + 1) grid
    , Map.lookup (x, y - 1) grid
    , Map.lookup (x, y + 1) grid
    , Map.lookup (x + 1, y - 1) grid
    , Map.lookup (x + 1, y) grid
    , Map.lookup (x + 1, y + 1) grid
    ]

occupiedCount :: Map (Int, Int) Space -> Int
occupiedCount = Map.foldl (\total space -> if space == SeatOccupied then total + 1 else total) 0