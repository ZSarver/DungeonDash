{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo, RankNTypes #-}
{-# LANGUAGE Arrows #-}
module Game where

import Keyboard (keyDownEvents, Key( .. ))
import Types
import SignalUtils
import Vector
import Random
import Event
import Enemy (enemiesInit, enemiesStep)
import Player (playerInit, playerStep)

--import FRP.Elerea.Param
import Control.Applicative
import Data.Traversable (sequenceA)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (NominalDiffTime)
import Control.Monad.State
import Control.Monad.Identity
import Control.Auto

--DEBUG SHIT KILL LATER
import GHC.Stack (errorWithStackTrace)
import System.Random (Random, random, randomR)

instance Random Key where
  random g = let (i,g') = randomR (0,3) g in ([UpKey,DownKey,LeftKey,RightKey] !! i, g')
  randomR _ g = random g

data GameState = GameState{ player :: !Player, enemies :: !Enemies }
gameInit = GameState playerInit enemiesInit

rkey :: Rand Key
rkey = fmap ([UpKey,DownKey,LeftKey,RightKey] !!) $ range (0,3)

newGame = undefined


--stepGame :: Auto m (inputs ...) GameState
stepGame :: Arrow a => a b b
stepGame =  proc x -> do
  returnA -< x  
  where
  player  = accum (\p (dt,e) -> playerStep dt e p) playerInit
  enemies = accum (\n (t,p,e) -> enemiesStep t p e n) enemiesInit
  --hits    = arr getHits
  --attacks = arr (\(keys, p, n) -> getAttacks keys p n)

{-
type Lens a b = forall f. Functor f => (b -> f b) -> a -> f a
class Embed s where
  lens :: Lens GameState s

view :: Lens a b -> a -> b
view l = getConst . l Const
set :: Lens a b -> a -> b -> a  
set l = l const

hoistState :: Lens s s' -> (forall a . State s' a -> State s a)
hoistState l = \x -> StateT $ \g -> 
  let (result, x') = runState x (view l g) 
  in Identity (result, set l g x')
-}
  
--hoist :: Auto (State s') a b -> Auto (State s) a b
--hoist = hoistA (hoistState lens)


--stepGame :: Auto (State GameState) a GameState 
--stepGame = undefined {-



{-
  proc (inputs ...) -> do
  
  
  
  where
 



  
-}

--State s t "=" s -> ((),s)
--playerStep "::" Time -> Events -> State Player ()
-- playerAuto :: Auto (State Player) (Time,Events) ()
-- playerLens :: Lens GameState Player
-- hoistA (hoistState playerLens) :: Auto (State Player) a b -> Auto (State GameState) a b
-- hoistA (hoistState playerLens) playerAuto :: Auto (State GameState) (Time, Events) ()


--transferAuto :: (a -> b -> b) -> Auto m 

{-
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

-}