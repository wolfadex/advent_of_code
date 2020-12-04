module Point exposing (Point, add, getX, getY, new, zero)


type Point
    = Point Int Int


new : { x : Int, y : Int } -> Point
new { x, y } =
    Point x y


zero : Point
zero =
    Point 0 0


add : Point -> Point -> Point
add (Point x1 y1) (Point x2 y2) =
    Point (x1 + x2) (y1 + y2)


getX : Point -> Int
getX (Point x _) =
    x


getY : Point -> Int
getY (Point _ y) =
    y
