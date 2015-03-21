{-# LANGUAGE RecordWildCards #-}
module Render where

import Types
import Vector
import Sfx

import FRP.Helm.Color
import FRP.Helm.Text (text, color, toText)
import FRP.Helm (toForm, move, rect, centeredCollage, filled, Element, group, Form)
import FRP.Helm.Graphics -- (outlined, solid, circle)
import Data.Maybe (maybeToList)
import Control.Applicative


render :: Int -> Int -> Player -> Enemies -> Sfx -> Element
render w' h' p e s = centeredCollage w' h' $ [background, draw s, drawZones p, drawEnemies (ppos p) e, drawPlayer p]
  where
  (w,h) = (fromIntegral w', fromIntegral h')
  background =  filled black $ rect w h
  
drawSymbol ::  Color -> Vec2 -> Char -> Form
drawSymbol c v a = move (toTuple v) . toForm . text . color c . toText $ [a]

drawEnemies :: Position -> Enemies -> Form
drawEnemies center = group . fmap (drawEnemy center) . list

drawEnemy :: Position -> Enemy -> Form
drawEnemy center Enemy{..} = group $ l ++ h ++ [drawSymbol grey (epos ^-^ center) echar]
  where
  h = maybeToList $ move (toTuple (epos ^-^ center)) 
      -- <$> liftA2 outlined (solid <$> highlight) (pure (circle 10))
      <$> liftA2 gradient (grd <$> highlight) (pure (circle 20))
  l = maybeToList $ 
         liftA2 traced
        (dashed <$> highlight)
        (pure $ path [ toTuple zero, toTuple (epos ^-^ center)])
  grd c@(Color r g b a) = radial (0,0) 0 (0,0) 20 [(0, rgba r g b (0.6 * a)), (1, rgba r g b 0)]
      

drawPlayer :: Player -> Form
drawPlayer Player{..} = drawSymbol white zero pchar

drawZones :: Player -> Form
drawZones Player{..} = --move (toTuple ppos)
  group 
  [ gradient (grd yellow) $ polygon (path up)
  , gradient (grd green) $ polygon (path down)
  , gradient (grd blue) $ polygon (path left)
  , gradient (grd red) $ polygon (path right)
  ]
  where
  grd c@(Color r g b a) = radial (0,0) 0 (0,0) zoneRadius [(0, rgba r g b 0), (0.5, rgba r g b 0), (1, rgba r g b 0.2)]
  f = (\t -> (zoneRadius * (cos t), zoneRadius * (sin t))) . (*pi)
  up =    range (5/4) (7/4) 30
  down =  range (1/4) (3/4) 30
  left =  range (3/4) (5/4) 30
  right = range (7/4) (9/4) 30
  path x = (0,0) : fmap f x
  range a b n = fmap (lerp a b . (/n)) [0..n]