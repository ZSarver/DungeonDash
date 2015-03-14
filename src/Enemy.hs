module Enemy where

import Types
import Vector


evolveNmeList :: Time -> Position -> [a] -> [Enemy] -> [Enemy]
evolveNmeList delta p keys l = newNmes ++ fmap (evolveNme delta p) l 
  where
  newNmes = if null keys then [] else [nme0 $ Vec2 100 100]

evolveNme :: Time -> Position -> Enemy -> Enemy
evolveNme delta p nme = setNmePos nme newpos
  where
    newpos = chaseStep 0.02 20 delta p $ getNmePos nme

getNmePos :: Enemy -> Position
getNmePos (Enemy (Character _ pos)) = pos
setNmePos :: Enemy -> Position  -> Enemy
setNmePos (Enemy (Character c _)) pos = Enemy $ Character c pos
      
nme0 v = Enemy $ Character 'k' v

chaseStep :: Double -> Double -> Time -> Position -> Position -> Position
chaseStep speed goalDistance time target start  = 
  if d > goalDistance then closer else start
  where 
    vec = target `minus` start
    d = distance start target
    closer = start `plus` ((speed * time / d) `scalarMult` vec)
