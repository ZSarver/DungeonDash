module Player where

import Types
import Vector

playerInit :: Player
playerInit = Player '@' zero

playerStep :: Time -> Events -> Player -> Player
playerStep dt events player = foldr handleEvent player events
  where
  handleEvent :: Event -> Player -> Player
  handleEvent e p = case e of
    HitEnemy Enemy{epos=t} -> p{ppos=t}
    otherwise              -> p

    

 