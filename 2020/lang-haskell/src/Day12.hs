module Day12 (solve1, solve2) where

import Debug.Trace

solve1 :: String -> Int
solve1 = manhattenDistance startPos . navigate (1, 0) startPos . lines
  where
    startPos = (0, 0)

solve2 :: String -> Int
solve2 = undefined

manhattenDistance :: (Int, Int) -> (Int, Int) -> Int
manhattenDistance (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2)

navigate :: (Int, Int) -> (Int, Int) -> [String] -> (Int, Int)
navigate _ endPos [] = Debug.Trace.traceShow endPos endPos
navigate (xf, yf) (xp, yp) (dir:rest) =
  case Debug.Trace.traceShow (xf, yf, xp, yp, dir) dir of
    'N' : amount -> navigate (xf, yf) (xp, yp + read amount) rest
    'S' : amount -> navigate (xf, yf) (xp, yp - read amount) rest
    'E' : amount -> navigate (xf, yf) (xp + read amount, yp) rest
    'W' : amount -> navigate (xf, yf) (xp - read amount, yp) rest
    'F' : amount ->
      case (xf, yf) of
        (0, 1) -> navigate (xf, yf) (xp, yp + read amount) rest -- North
        (0, -1) -> navigate (xf, yf) (xp, yp - read amount) rest -- South
        (1, 0) -> navigate (xf, yf) (xp + read amount, yp) rest -- East
        (-1, 0) -> navigate (xf, yf) (xp - read amount, yp) rest -- West
        _ -> Debug.Trace.trace "shouldn't be here either" navigate (xf, yf) (xp, yp) rest
    'R' : amount -> navigate (rotate (xf, yf) (read amount)) (xp, yp) rest
    'L' : amount -> navigate (rotate (xf, yf) (-(read amount))) (xp, yp) rest

rotate :: (Int, Int) -> Int -> (Int, Int)
rotate (x, y) amount =
  case (abs amount, x, y) of
    (180, _, _) -> (- x, - y)
    (90, 1, 0) -> (0, -1 * signum amount)
    (90, -1, 0) -> (0, 1 * signum amount)
    (270, 1, 0) -> (0, 1 * signum amount)
    (270, -1, 0) -> (0, -1 * signum amount)
    (90, 0, 1) -> (1 * signum amount, 0)
    (90, 0, -1) -> (-1 * signum amount, 0)
    (270, 0, 1) -> (-1 * signum amount, 0)
    (270, 0, -1) -> (1 * signum amount, 0)
    _ -> Debug.Trace.trace "shouldn't be here" (x, y)
