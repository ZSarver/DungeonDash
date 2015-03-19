module Event where

import Types
import Keyboard (Key(..))
import Vector
import Data.List (sortBy)
import Data.Ord (comparing)

eventsInit :: Events
eventsInit = []

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
eventsStep _ keys p e _ = getHits keys p e


getHits keys p e = if null keys then [] 
  else case head keys of
    UpKey    -> hit UpZone
    DownKey  -> hit DownZone
    LeftKey  -> hit LeftZone
    RightKey -> hit RightZone
    _        -> []
  where
  hit z = fmap (HitEnemy . fst) $ filter ((z==) . snd) $ (targets p e)
    
targets :: Player -> Enemies -> [(Enemy, Zone)]
targets Player{ppos=p, zoneRadius=zr} Enemies{list=elist} = concat
  . zipWith (flip zip) [[UpZone],[DownZone],[LeftZone],[RightZone]]
  . fmap (sortBy (comparing $ distance p . epos))
  $ [up, down, left, right]
  where
  (up,down,left,right,out) = foldr split ([],[],[],[],[]) $ zip elist $ fmap (getZone zr p . epos) elist
  split (e,UpZone   ) (u,d,l,r,o) = (e:u,  d,  l,  r,  o)
  split (e,DownZone ) (u,d,l,r,o) = (  u,e:d,  l,  r,  o)
  split (e,LeftZone ) (u,d,l,r,o) = (  u,  d,e:l,  r,  o)
  split (e,RightZone) (u,d,l,r,o) = (  u,  d,  l,e:r,  o)
  split (e,OutZone  ) (u,d,l,r,o) = (  u,  d,  l,  r,e:o)
  
