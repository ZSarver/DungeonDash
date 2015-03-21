{-# LANGUAGE RecordWildCards #-}
module Event where

import Types
import Keyboard (Key(..))
import Vector
import Data.List (sortBy,transpose,find)
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Control.Applicative

eventsInit :: Events
eventsInit = []



getHits Player{pact=a} = case a of
  Flying{ .. } -> if flyElapsed >= flyDuration then [HitEnemy flyTo] else []
  _            -> []

getAttacks keys p e = if null keys || (pact p /= Waiting) then [] 
  else case head keys of
    UpKey    -> hit UpZone
    DownKey  -> hit DownZone
    LeftKey  -> hit LeftZone
    RightKey -> hit RightZone
    _        -> []
  where
  hit z = fmap (AttackEnemy . fst) $ filter ((z==) . snd) $ (targets p e)

--The targetting scheme from start to finish
--
-- We prefer to target CLOSE enemies over FAR enemies,
--   and we prefer for different buttons to be assigned to different enemies
--   but if we can't have that it's OK for multiple buttons to collide
-- 1) For each enemy lazily compute: distance, u, v for each enemy
--                                     AND 4 zone eligibilities
-- 2) Filter the list to eligible targets
-- 3) For each zone, select the closest enemy for which this is their 
--        preferred zone.
-- 4) If any zones get unfilled by this first pass, fill them with the closest
--       eligible _unassigned_ enemy.
-- 5) If we still have unfilled zones, fill them with the closes eligible enemy.
--There is probably a smarter way to do this.
targets :: Player -> Enemies -> [(Enemy, Zone)]
targets pl@Player{ppos=p, zoneRadius=zr} Enemies{list=elist} =
    (fmap.map1) fromJust
  $ filter ((/=Nothing) . fst) 
  $ flip zip zones 
  $ zipWith (<|>) pass1 $ zipWith (<|>) pass2 pass3
  where
  pass1 :: [Maybe Enemy]
  pass1 = fmap (find (const True) . fmap one . zoneprefs) $ zones
  pass2 = fmap (find (const True) . fmap one . zonebackups1) $ zones
  pass3 =  fmap (find (const True) . fmap one . zonebackups2) $ zones
  zoneprefs z = filter ((==z).three) prep
  zonebackups1 z = filter (\x -> not $ (Just (one x)) `elem` pass1) $ zonebackups2 z
  zonebackups2 z = filter ((!! fromEnum z).four) . filter ((z/=).three) $ prep
  prep = sortBy (comparing two) . filter ((<= zr) . two) $ fmap f elist
  f :: Enemy -> (Enemy, Double, Zone, [Bool])
  f nme@Enemy{epos=e} = let relpos@(Vec2 x y) = e `minus` p 
                            u = x + y
                            v = x - y
                            d = distance relpos zero
                        in (nme,d,prefZone u v,eligibleZones u v x y)
  one   = (\(x,_,_,_) -> x)
  two   = (\(_,x,_,_) -> x)
  three = (\(_,_,x,_) -> x)
  four  = (\(_,_,_,x) -> x)
  map1 f (a,b) = (f a , b)
  zones = [UpZone, DownZone, LeftZone, RightZone]
-- u = dx + dy, v = dx - dy
prefZone u v = case (v > 0, u > 0) of
  (True,False)  -> UpZone
  (False,True)  -> DownZone
  (False,False) -> LeftZone
  (True,True)   -> RightZone
                                              --              |      v
eligibleZones u v x y = ([up,down,left,right])--              |    /
  where                                       --              |  /
  up    = and [ -u > flex,  v > flex, -y > 0] --     ------------------- x
  down  = and [  u > flex, -v > flex,  y > 0] --              |  
  left  = and [ -u > flex, -v > flex, -x > 0] --              |   \
  right = and [  u > flex,  v > flex,  x > 0] --              |     \
  flex = -60                                  --              y       u

