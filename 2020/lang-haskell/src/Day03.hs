{-# LANGUAGE NamedFieldPuns #-}

module Day03 (solve1, solve2) where

import Data.Array.IArray (Array, (!))
import qualified Data.Array.IArray as Array
import Data.List.Split (splitOn)
import Point (Point)
import qualified Point

solve1 :: String -> Int
solve1 = rideTheToboggan (Point.new 3 1) Point.zero 0 . buildConfig

solve2 :: String -> Int
solve2 input =
  route11 * route31 * route51 * route71 * route12
  where
    baseData = buildConfig input
    route11 = rideTheToboggan (Point.new 1 1 ) Point.zero 0 baseData
    route31 = rideTheToboggan (Point.new 3 1 ) Point.zero 0 baseData
    route51 = rideTheToboggan (Point.new 5 1 ) Point.zero 0 baseData
    route71 = rideTheToboggan (Point.new 7 1 ) Point.zero 0 baseData
    route12 = rideTheToboggan (Point.new 1 2 ) Point.zero 0 baseData
              
              

---- TYPES ----

data Config = Config
  { width :: Int
  , height :: Int
  , grid :: Array Int Char
  }

---- CORE FUNCTIONS ----


buildConfig :: String -> Config
buildConfig gridStr =
  Config w h g
  where
    rows = lines gridStr
    w = length $ head rows
    h = length rows
    g = Array.listArray (0, w * h) $ concat $ lines gridStr

rideTheToboggan :: Point -> Point -> Int -> Config -> Int
rideTheToboggan moveDir pos treesHit config@Config { width, height, grid } =
  if Point.getY adjustedPos >= height then
    treesHit
  else
    let newTreesHit = if isTree grid width adjustedPos then treesHit + 1 else treesHit
    in rideTheToboggan moveDir adjustedPos newTreesHit config
  where
    newPos = Point.add moveDir pos
    adjustedPos = if Point.getX newPos >= width then Point.add (Point.new (-width) 0) newPos else newPos

isTree :: Array Int Char -> Int -> Point -> Bool
isTree grid width pos =
  sym == '#'
  where
    x = Point.getX pos
    y = Point.getY pos
    idx = y * width + x
    sym = grid ! idx
