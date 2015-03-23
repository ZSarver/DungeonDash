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
import System.Random (Random, random, randomR)

instance Random Key where
  random g = let (i,g') = randomR (0,3) g in ([UpKey,DownKey,LeftKey,RightKey] !! i, g')
  randomR _ g = random g

data GameState = GameState{ player :: !Player, enemies :: !Enemies, sfx :: !Sfx }
gameInit = GameState playerInit enemiesInit sfxInit

rkey :: Rand Key
rkey = fmap ([UpKey,DownKey,LeftKey,RightKey] !!) $ range (0,3)
rkeys :: Signal (Rand [Key])
rkeys = pure . fmap pure $ rkey

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
  let rng = mkStdGen seed
  player <- transfer playerInit playerStep events
  enemies <- transfer2 enemiesInit enemiesStep player events
  keys <- evalRandomSignal rng rkeys -- keyDownEvents [UpKey,DownKey,LeftKey,RightKey,SpaceKey]
  spawns <- evalRandomSignal rng =<< liftA2 spawnWhen (every 500) (pure player)
  let attacks = getAttacks <$> keys <*> player <*> enemies
      hits = getHits <$> player
--  debug <- transfer [] debugfun keys
  events <- delay eventsInit . fmap concat . sequenceA $ 
    [ attacks
    , spawns
    , hits
--    , debug
    ]
  sfx <- transfer3 sfxInit sfxStep player enemies events
  memo $ liftA3 GameState player enemies sfx
  
debugfun :: Time -> [Key] -> [Event] -> [Event]
debugfun t ks es = if SpaceKey `elem` ks 
                   then errorWithStackTrace "boop"
                   else []

randomPosition :: Rand Position
randomPosition = Vec2 <$> range (-1000,1000) <*> range (-1000,1000)

isOut :: Player -> Position -> Bool
isOut Player{zoneRadius=r,ppos=p} here = distance p here > r

spawnWhen :: Signal Bool -> Signal Player -> Signal (Rand [Event])
spawnWhen s p =  f <$> s <*> p
  where
  f True p = (\pos -> if isOut p pos then [SpawnEnemy pos] else []) <$> (randomPosition)
  f False p = pure []

