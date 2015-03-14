{-# LANGUAGE RecordWildCards #-}

import Keyboard (defaultKeyDownEvents, keyDownEvents, Key( .. ))
import Types
import Render
import SignalUtils
import Vector
import Enemy

import FRP.Helm
import Control.Applicative
import FRP.Helm.Time(fps)
import FRP.Helm.Random (float)
import FRP.Helm.Sample (value, Sample (..))
import Data.Traversable (sequenceA)
import FRP.Elerea.Param (effectful, transfer2, transfer,snapshot,delay)
import Data.IORef (newIORef, readIORef, modifyIORef, IORef)


enemies :: Signal [Enemy]
enemies = foldp3' evolveNmeList [] clock loop (keyDownEvents [SpaceKey])
  where
  clock = unmask (keyDownEvents [SpaceKey]) gameClock

pc :: Signal Character
pc = Character '@' <~ loop
--pc = pure $ Character '@' (0,0)


getZone :: Double -> Position -> Position -> Zone
getZone radius playerPos enemyPos = 
  let d = distance playerPos enemyPos 
      Vec2 v1 v2 = enemyPos `minus` playerPos
  in   if d > radius then OutZone
       else case (v1 - v2 > 0, v1 + v2 > 0  ) of
            --   (top-right? , bottom-right?)
            (True,True)   -> RightZone
            (True,False)  -> UpZone
            (False,True)  -> DownZone
            (False,False) -> LeftZone

    
gameClock = fps 30
gameClockElapsed = foldp (+) 0 gameClock
loop :: Signal Vec2
loop = let r= 50; z = 500 in fmap (\t -> (Vec2 (r * cos (t/z)) (r * sin (t/z)))) gameClockElapsed
--simulation :: H.Signal (SignalGen Engine (Elerea.Signal (Sample Element)))
simulation = GameState <~ pc ~~ enemies

  
main = do
    run config $ render 800 600 <~ simulation
  where
    config = defaultConfig { windowTitle = "Dungeon Dash!", windowPosition = (200,200) }