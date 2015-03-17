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
getNmePos = epos
setNmePos :: Enemy -> Position -> Enemy
setNmePos e pos = e{epos=pos}
      
nme0 v = Enemy 'k' v 0

chaseStep :: Double -> Double -> Time -> Position -> Position -> Position
chaseStep speed goalDistance time target start  = 
  if d > goalDistance then closer else start
  where 
    vec = target `minus` start
    d = distance start target
    closer = start `plus` ((speed * time / d) `scalarMult` vec)
