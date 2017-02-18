import Data.List

data Direction = LeftDir | RightDir | StraightDir deriving (Show)

data Point = Point Double Double
             deriving (Show)

subPoint (Point ax ay) (Point bx by) = Point (ax - bx) (ay - by)
projectAngle (Point x y) = atan (y / x)
getAngle :: Point -> Point -> Point -> Double
getAngle a b c = projectAngle v1 - projectAngle v2
                    where v1 = subPoint a b
                          v2 = subPoint c b
getDir a b c = if angleDiff > 0 then  RightDir
               else if angleDiff < 0 then LeftDir
               else StraightDir
               where angleDiff = (getAngle a b c)

computeDirections :: [Point] -> [Direction]
computeDirections (p1:p2:p3:ps) = [(getDir p1 p2 p3)] ++ (computeDirections ([p2, p3] ++ ps))
computeDirections _             = []

pointsCompare (Point x1 y1) (Point x2 y2) = if (compare y1 y2) /= EQ then compare y1 y2 
                                            else compare x1 x2

pointsSort pts = sortBy pointsCompare pts
