module Sfx where

import Types
import Event
import Vector

import FRP.Helm.Graphics
import FRP.Helm.Color

data Sfx = Sfx { draw :: !Form
               , sfxt :: !Time
               }
               
sfxInit = Sfx emptyForm 0

emptyForm = group []

sfxStep :: Time -> Player -> Enemies -> Events -> Sfx -> Sfx
sfxStep dt pl e@Enemies{list=nmes} _ Sfx{sfxt = sfxt} = Sfx{draw = toDraw, sfxt = t'}
  where
  t' = dt + sfxt
  f (e,z) = zoneLine z (epos e ^-^ ppos pl)
  toDraw = group $ fmap f $ targets pl e

zoneLine :: Zone -> Position -> Form
zoneLine z p = traced (dashed $ zoneColor z) zoneArc
  where zoneArc = fmap toTuple 
                $ arc (interpolate2 zero (20 *^ zoneDirection z) p) 0 1 30

arc :: (Double -> a) -> Double -> Double -> Int -> [a]
arc f start end numsamples = let n = fromIntegral numsamples in 
  fmap (f . lerp start end . (/n)) [0..n]
        
zoneColor z = colorlist !! fromEnum z
colorlist = [yellow, green, blue, red, (rgba 0 0 0 0)]
zoneDirection z = directions !! fromEnum z
directions = [ Vec2 0 (-1), Vec2 0 1, Vec2 (-1) 0, Vec2 1 0]

interpolate2 start start' end t = (2*t*(1-t)) *^ start'
                              ^+^ (1-t**2)    *^ start
                              ^+^ (t**2)      *^ end
