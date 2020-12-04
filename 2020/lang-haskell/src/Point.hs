module Point (Point, new, add, getX, getY, zero) where

data Point = Point Int Int

new :: Int -> Int -> Point
new =
  Point

zero :: Point
zero = Point 0 0

add :: Point -> Point -> Point
add (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

getX :: Point -> Int
getX (Point x _) = x

getY :: Point -> Int
getY (Point _ y) = y