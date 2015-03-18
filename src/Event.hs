module Event where

import Types
import Keyboard (Key(..))
import Vector
import Data.List (sortBy)
import Data.Ord (comparing)

eventsInit :: Events
eventsInit = []

 -- TODO: this should become a function of Player if it starts to vary during the game
zoneRadius = 100

--Eventually we want the zones to be color coded.
--I suggest we match the colors of the xbox controller 
-- buttons for easier association
--     Y
--    B R
--     G

getZone :: Double -> Position -> Position -> Zone
getZone radius playerPos enemyPos = 
  let d = distance playerPos enemyPos 
      Vec2 v1 v2 = enemyPos `minus` playerPos
  in   if d > radius then OutZone
       else case (v1 - v2 > 0, v1 + v2 > 0  ) of
            --   (top-right? , bottom-right?)
            (True,False)  -> UpZone
            (False,True)  -> DownZone
            (False,False) -> LeftZone
            (True,True)   -> RightZone

eventsStep :: Time -> [Key] -> Player -> Enemies -> Events -> Events
eventsStep _ keys Player{ppos=p} Enemies{list=elist} _ = if null keys then [] 
  else case head keys of
    UpKey    -> hit up
    DownKey  -> hit down
    LeftKey  -> hit left
    RightKey -> hit right
    otherwise -> []
  where
  hit l = case l of
    []    -> []
    (e:_) -> [HitEnemy e]
  (up, down, left, right, out) = let s = sortBy (comparing $ distance p . epos) in (s up',s down',s left',s right',s out')
  (up',down',left',right',out') = foldr split ([],[],[],[],[]) $ zip elist $ fmap (getZone zoneRadius p . epos) elist
  split (e,UpZone   ) (u,d,l,r,o) = (e:u,  d,  l,  r,  o)
  split (e,DownZone ) (u,d,l,r,o) = (  u,e:d,  l,  r,  o)
  split (e,LeftZone ) (u,d,l,r,o) = (  u,  d,e:l,  r,  o)
  split (e,RightZone) (u,d,l,r,o) = (  u,  d,  l,e:r,  o)
  split (e,OutZone  ) (u,d,l,r,o) = (  u,  d,  l,  r,e:o)