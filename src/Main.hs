{-# LANGUAGE RecordWildCards #-}
import Keyboard (clockedKeyDownEvents, Key( .. ))
import Types
import Render
import SignalUtils
import Vector
import Enemy
import Event

import FRP.Helm
import Control.Applicative
import FRP.Helm.Time(fps,second)
import FRP.Helm.Random (float)
import FRP.Helm.Sample (value, Sample (..))
import Data.Traversable (sequenceA)
import FRP.Elerea.Param (effectful, transfer2, transfer,snapshot,delay)
import Data.IORef (newIORef, readIORef, modifyIORef, IORef)
{-
So here's the plan:
We'll have a few top level signals with fixed dependencies. All signals will
be created at initialization and destroyed when things end. Since the 
dependency network is fixed, we will never end up with wacky types of the form 
"Signal Signal a".
 - ENEMIES 
    -- describes the current state of all enemies
    -- depends on EVENTS
              and PLAYER
 - PLAYER
    -- describes the current state of the player
    -- depends on EVENTS
 - EVENTS
    -- describes interactions that have consequences on multiple signals,
       including (but maybe not limited to?) attacks
    -- depends on ENEMIES
              and PLAYER
              and INPUT
 - INPUT
   -- keyboard, mouse, game clock, etc
   -- no dependencies on any part of the signal network
 - DISPLAY
   -- What gets drawn to the screen
   -- depends on PLAYER
             and ENEMIES
   -- since only main depends on this, we don't need to give a 
      name to the signal
 - maybe more later? this is enough for now.
    - eventually we may want a signal for the CAMERA, 
       if it isn't always just centered on the player
    - we may also want signals for ITEMS, BLOOD_SPLATTERS, 
       SIMULATION_SPEED_MULTIPLIER, or other special effects
-}
{-

data Enemies = Enemies ()
enemiesInit :: Enemies
enemiesStep :: Time -> Player -> Events -> Enemies -> Enemies
enemies :: Signal Enemies

data Player = Player ()
playerInit :: Player
playerStep :: Time -> Events -> Player -> Player
player :: Signal Player

data Events = Events ()
eventsInit :: Events
eventsStep :: [Key] -> Player -> Enemies -> Events -> Events
events :: Signal Events

display :: Enemies -> Player -> Element


-- input doesn't need a single dedicated signal



-}

events :: Signal Events
events = foldp3' eventsStep eventsInit keys pc enemies
  where
  keys = clockedKeyDownEvents gameClock [UpKey, DownKey, LeftKey, RightKey]


enemies' :: Signal [Enemy]
enemies' = foldp3' evolveNmeList [] gameClock loop (clockedKeyDownEvents gameClock [SpaceKey])

enemies = fmap Enemies enemies'
  
pc :: Signal Player
pc = Player '@' <~ loop
--pc = pure $ Character '@' (0,0)




gameClock = fps' 30
  where 
    f y (Changed x) = Changed y
    f _ (Unchanged _) = Unchanged 0
    fps' k = Signal $ (fmap.fmap) (f (second / k)) $ signalGen $ fps k

gameClockElapsed = foldp (+) 0 gameClock
loop :: Signal Vec2
loop = let r= 50; z = 500 in fmap (\t -> (Vec2 (r * cos (t/z)) (r * sin (t/z)))) gameClockElapsed


main = do
    run config $ render 800 600 <~ pc ~~ enemies
  where
    config = defaultConfig { windowTitle = "Dungeon Dash!", windowPosition = (200,200) }