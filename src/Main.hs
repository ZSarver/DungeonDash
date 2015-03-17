{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

import Keyboard (keyDownEvents, Key( .. ))
import Types
import Render
import SignalUtils
import Vector
import Enemy (enemiesInit, enemiesStep)
import Event (eventsInit, eventsStep)
import Player (playerInit, playerStep)

import FRP.Helm
import Control.Applicative
import FRP.Helm.Time(fps,second)
import FRP.Helm.Random (range)
import FRP.Helm.Sample (value, Sample (..))
import Data.Traversable (sequenceA)
import FRP.Elerea.Param 
  ( effectful, transfer2, transfer
  , transfer3, snapshot,delay, execute
  , start, stateful)
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

{-
spawns = spawnOnSpace
spawnOnSpace = spawnWith $ (not.null) <~ clockedKeyDownEvents gameClock [SpaceKey]
spawnWith :: Signal Bool -> Signal Events
-}
spawnWhen s = fmap f s
  where
  f False = []
  f True = [SpawnEnemy (Vec2 100 100)]

(+-+) = liftA2 (++)


elapsedTime = stateful 0 (+)

game = start $ mdo
  let events = events1 +-+ spawnWhen spaceDown
      spaceDown = elem SpaceKey <$> keys
  keys <- keyDownEvents [UpKey,DownKey,LeftKey,RightKey,SpaceKey]
  enemies <- transfer2 enemiesInit enemiesStep player events
  player <- transfer playerInit playerStep events
  events1 <- delay eventsInit =<< transfer3 eventsInit eventsStep keys player enemies
  return $ fmap pure $ liftA2 (,) enemies player

main = do
  print "aa"
  stepGame <- game
  let game' = Signal $ effectful (stepGame 0.8)
      enemies = fmap fst game'
      player = fmap snd game'
  run config $ refresh $ render 800 600 <~ player ~~ enemies
  where
    config = defaultConfig { windowTitle = "Dungeon Dash!", windowPosition = (200,200) }
    refresh s = const <~ s ~~ (fps 40)
