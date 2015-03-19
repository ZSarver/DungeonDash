{-# LANGUAGE RecordWildCards #-}
module Render where

import Types
import Vector

import FRP.Helm.Color
import FRP.Helm.Text (text, color, toText)
import FRP.Helm (toForm, move, rect, centeredCollage, filled, Element, group, Form)
import FRP.Helm.Graphics (outlined, solid, circle)
import Data.Maybe (maybeToList)
import Control.Applicative


render :: Int -> Int -> Player -> Enemies -> Element
render w' h' p e = centeredCollage w' h' $ [background, drawEnemies e, drawPlayer p]
  where
  (w,h) = (fromIntegral w', fromIntegral h')
  background =  filled black $ rect w h
  
drawSymbol ::  Color -> Vec2 -> Char -> Form
drawSymbol c v a = move (toTuple v) . toForm . text . color c . toText $ [a]

drawEnemies :: Enemies -> Form
drawEnemies = group . fmap drawEnemy . list

drawEnemy :: Enemy -> Form
drawEnemy Enemy{..} = group $ h ++ [drawSymbol grey epos echar]
  where
  h = maybeToList $ move (toTuple epos) 
      <$> liftA2 outlined (solid <$> highlight) (pure (circle 10))

drawPlayer :: Player -> Form
drawPlayer Player{..} = drawSymbol white ppos pchar