module Render where

import Types

import FRP.Helm.Color
import FRP.Helm.Text (text, color, toText)
import FRP.Helm (toForm, move, rect, centeredCollage, filled, Element)


render :: Int -> Int -> GameState -> Element
render w' h' g = centeredCollage w' h' (background : drawCharacter red p : fmap (drawCharacter green) e)
  where
  GameState p e _ = g
  (w,h) = (fromIntegral w', fromIntegral h')
  drawSymbol a c = toForm . text . color c . toText $ [a]
  drawCharacter col c = let (Character a p) = toCharacter c in move p (drawSymbol a col)
  background =  filled black $ rect w h
  
class ToCharacter a where
  toCharacter :: a -> Character
  
instance ToCharacter Character where
  toCharacter = id
  
instance ToCharacter Enemy where
  toCharacter = chr
