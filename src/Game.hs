{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
module Game where

import Keyboard (keyDownEvents, Key( .. ))
import Types
import SignalUtils
import Vector
import Random
import Event
import Enemy (enemiesInit, enemiesStep)
import Player (playerInit, playerStep)

import FRP.Elerea.Param
import Control.Applicative
import Data.Traversable (sequenceA)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (NominalDiffTime)


data GameState = GameState{ player :: Player, enemies :: Enemies }
gameInit = GameState playerInit enemiesInit


--Arguments: game step resolution, game seed
newGame :: NominalDiffTime -> Int -> SignalGen p (Signal GameState)
newGame dt seed = do
  gameStep <- execute $ start (game seed)
  t <- timeTicks dt
  r <- effectful1 (stepIf gameStep) t
  accumMaybes gameInit r
  where
  stepIf f b = if b 
    then fmap Just (f $ 1000 * realToFrac dt) 
    else return Nothing

game :: Int -> SignalGen Time (Signal GameState)
game seed = mdo
  rng <- liftIO $ mkRng seed
  events <- memo . fmap concat . sequenceA $ 
    [ attacks
    , spawns
    , hits
    ]
  keys <- keyDownEvents [UpKey,DownKey,LeftKey,RightKey,SpaceKey]
  enemies <- transfer2 enemiesInit enemiesStep player events
  player <- transfer playerInit playerStep events
  attacks <- delay eventsInit $ getAttacks <$> keys <*> player <*> enemies
  hits <- delay eventsInit $ getHits <$> player
  spawns <- evalRandomSignal rng =<< fmap spawnWhen (every 1000)
  memo $ liftA2 GameState player enemies

randomPosition :: Rand Position
randomPosition = Vec2 <$> range (-500,500) <*> range (-500,500)

spawnWhen :: Signal Bool -> Signal (Rand [Event])
spawnWhen s = fmap f s
  where
  f True = (\p -> [SpawnEnemy p]) <$> randomPosition
  f False = pure []
