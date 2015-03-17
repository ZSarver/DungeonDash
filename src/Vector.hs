module Vector where

data Vec2 = Vec2 !Double !Double deriving (Eq)
type Position = Vec2

zero :: Vec2
zero = Vec2 0 0

vec2 :: (Double, Double) -> Vec2
vec2 (x,y) = Vec2 x y

plus :: Vec2 -> Vec2 -> Vec2
plus (Vec2 a1 a2) (Vec2 b1 b2) = Vec2 (a1 + b1) (a2 + b2)

minus :: Vec2 -> Vec2 -> Vec2
minus (Vec2 a1 a2) (Vec2 b1 b2) = Vec2 (a1 - b1) (a2 - b2)

distance :: Position -> Position -> Double
distance (Vec2 a1 a2) (Vec2 b1 b2) = sqrt $ (a1-b1)^2 + (a2-b2)^2

scalarMult :: Double -> Vec2 -> Vec2
scalarMult s (Vec2 a b) = Vec2 (s * a) (s * b)

toTuple :: Vec2 -> (Double,Double)
toTuple (Vec2 x y) = (x,y)
{-

type V2 = Position
minus :: Position -> Position -> V2
minus (a1,a2) (b1,b2) = (a1-b1,a2-b2)
plus :: Position -> V2 -> Position
plus (a1,a2) (b1,b2) = (a1+b1,a2+b2)


distance :: Position -> Position -> Double
distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2
-}