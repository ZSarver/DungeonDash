{-# LANGUAGE TypeFamilies #-}
module Vector ((^-^),(^+^),(*^),zero,Vec2(..),toTuple,Position, distance, lerp) where
import System.Random (Random, random, randomR)
import Data.VectorSpace

data Vec2 = Vec2 !Double !Double deriving (Eq)
type Position = Vec2

instance Random Vec2 where
  random g1 = let (x,g2) = random g1
                  (y,g3) = random g2
              in (Vec2 x y, g3)
  randomR (Vec2 x1 y1, Vec2 x2 y2) g1 = 
    let (x,g2) = randomR (x1,x2) g1
        (y,g3) = randomR (y1,y2) g2
    in (Vec2 x y, g3)

    
instance AdditiveGroup Vec2 where
  zeroV = zero
  (^+^) (Vec2 a1 a2) (Vec2 b1 b2) = Vec2 (a1 + b1) (a2 + b2)
  negateV (Vec2 b1 b2) = Vec2 (-b1) (-b2)

instance VectorSpace Vec2 where
  type Scalar Vec2 = Double
  (*^) c (Vec2 x y) = Vec2 (c*x) (c*y)
  
zero :: Vec2
zero = Vec2 0 0

vec2 :: (Double, Double) -> Vec2
vec2 (x,y) = Vec2 x y

distance :: Position -> Position -> Double
distance (Vec2 a1 a2) (Vec2 b1 b2) = sqrt $ (a1-b1)^2 + (a2-b2)^2

toTuple :: Vec2 -> (Double,Double)
toTuple (Vec2 x y) = (x,y)