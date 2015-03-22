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
import Sfx

import FRP.Elerea.Param
import Control.Applicative
import Data.Traversable (sequenceA)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (NominalDiffTime)

--DEBUG SHIT KILL LATER
import GHC.Stack (errorWithStackTrace)


data GameState = GameState{ player :: Player, enemies :: Enemies, sfx :: Sfx }
gameInit = GameState playerInit enemiesInit sfxInit

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
  let attacks = getAttacks <$> keys <*> player <*> enemies
      hits = getHits <$> player
  spawns <- evalRandomSignal rng =<< liftA2 spawnWhen (every 1000) (pure player)
  events <- delay eventsInit . fmap concat . sequenceA $ 
    [ attacks
    , spawns
    , hits
    , debug
    ]
  keys <- keyDownEvents [UpKey,DownKey,LeftKey,RightKey,SpaceKey]
  enemies <- transfer2 enemiesInit enemiesStep player events
  player <- transfer playerInit playerStep events
  sfx <- transfer3 sfxInit sfxStep player enemies events
  debug <- transfer [] debugfun keys
  memo $ liftA3 GameState player enemies sfx
  
debugfun :: Time -> [Key] -> [Event] -> [Event]
debugfun t ks es = if SpaceKey `elem` ks 
                   then errorWithStackTrace "boop"
                   else []

randomPosition :: Rand Position
randomPosition = Vec2 <$> range (-600,600) <*> range (-600,600)

isOut :: Player -> Position -> Bool
isOut Player{zoneRadius=r,ppos=p} here = distance p here > r

spawnWhen :: Signal Bool -> Signal Player -> Signal (Rand [Event])
spawnWhen s p =  f <$> s <*> p
  where
  f True p = (\pos -> if isOut p pos then [SpawnEnemy pos] else []) <$> (randomPosition)
  f False p = pure []

