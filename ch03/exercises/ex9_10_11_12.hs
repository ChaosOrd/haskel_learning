import Data.List

data Direction = LeftDir | RightDir | StraightDir deriving (Show, Eq)

data Point = Point Double Double
             deriving (Show)

subPoint (Point ax ay) (Point bx by) = Point (ax - bx) (ay - by)

projectAngle (Point x y) = atan (y / x) * signum y

getAngle a b c = projectAngle v1 - projectAngle v2
                    where v1 = subPoint a b
                          v2 = subPoint c b

getDir a b c = if angle > 0 then  RightDir
               else if angle < 0 then LeftDir
               else StraightDir
               where angle = getAngle a b c

computeDirections (p1:p2:p3:ps) = [(getDir p1 p2 p3)] ++ (computeDirections ([p2, p3] ++ ps))
computeDirections _             = []

pointsCompare (Point x1 y1) (Point x2 y2) = if (compare y1 y2) /= EQ then compare y1 y2 
                                            else compare x1 x2

firstPoint pts = minimumBy pointsCompare pts

pointsCompareByAngles p1 p2 = compare (projectAngle p1) (projectAngle p2)

pointsSort pts = sortBy pointsCompareByAngles pts

corvexHull pts = (selectPoints . pointsSort) pts
                 where selectPoints (p1:p2:p3:pts) | (getDir p1 p2 p3) == RightDir = corvexHull (p1:p3:pts)
                       selectPoints (p1:p2:p3:pts)                                 = p1:(corvexHull (p2:p3:pts))
                       selectPoints (p1:p2:[])                                     = p1:p2:[]
                       selectPoints (p1:[])                                        = p1:[]
                       selectPoints []                                             = []
