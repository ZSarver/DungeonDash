{-# LANGUAGE RecordWildCards #-}
module Player where

import Types
import Vector

playerInit :: Player
playerInit = Player "some filepath" zero 150 Waiting

flySpeed = 0.4

playerStep :: Time -> Events -> Player -> Player
playerStep dt events player = 
  handleEvents
  . tick
  $ player 
  where
  handleEvents p = foldr handleEvent p events
  handleEvent :: Event -> Player -> Player
  handleEvent e p = case e of
    AttackEnemy nme -> fly p nme (distance (ppos p) (epos nme) / flySpeed)
    HitEnemy nme    -> p{pact = Waiting}
    _               -> p
  tick p = case (pact p) of
    Flying{ .. } -> fly' p dt
    _ -> p

fly p nme t = p{pact = Flying (ppos p) nme t 0}
fly' p@Player{pact = a} dt = 
  let elapsed' = flyElapsed a + dt
      a' = a{flyElapsed = elapsed'}
      pos' = lerp (flyFrom a) (epos . flyTo $ a) (flyElapsed a / flyDuration a)
  in  p{pact = a', ppos = pos'}