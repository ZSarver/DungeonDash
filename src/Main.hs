{-# LANGUAGE RecordWildCards #-}

import Keyboard (defaultKeyDownEvents, keyDownEvents, Key( .. ))
import Types
import Render
import SignalUtils

import FRP.Helm
import Control.Applicative
import FRP.Helm.Time(fps)
import FRP.Helm.Random (float)
import FRP.Helm.Sample (value, Sample (..))
import Data.Traversable (sequenceA)
import FRP.Elerea.Param (effectful, transfer2, transfer,snapshot,delay)
import Data.IORef (newIORef, readIORef, modifyIORef, IORef)



pc :: Signal Character
pc = Character '@' <~ loop
--pc = pure $ Character '@' (0,0)

enemy1 :: Signal Enemy
enemy1 = fmap Enemy $ Character 'k' <~ chase 3 60 (60,60) (fmap position pc)

enemy2 = fmap Enemy $ Character 'k' <~ chase 4 20 (0,60) (fmap position pc)

newEnemy :: Double -> Double -> Position -> Signal Enemy
newEnemy speed dist pos = fmap Enemy $ Character 'k' <~ chase speed dist pos (fmap position pc)

chase :: Double -> Double -> Position -> Signal Position -> Signal Position
chase speed goalDistance start target = foldp2' step start gameClock target
  where 
    step time to@(t1,t2) from@(f1,f2) = 
      let (v1,v2) = to `minus` from
          d = distance from to
          (u1,u2) = (v1 / d, v2 / d)
          closer = from `plus` (speed * u1, speed * u2)
      in  if d > goalDistance then closer else from

distance :: Position -> Position -> Double
distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

type V2 = Position
minus :: Position -> Position -> V2
minus (a1,a2) (b1,b2) = (a1-b1,a2-b2)
plus :: Position -> V2 -> Position
plus (a1,a2) (b1,b2) = (a1+b1,a2+b2)

getZone :: Double -> Position -> Position -> Zone
getZone radius playerPos enemyPos = 
  let d = distance playerPos enemyPos 
      (v1,v2) = enemyPos `minus` playerPos
  in   if d > radius then OutZone
       else case (v1 - v2 > 0, v1 + v2 > 0  ) of
            --   (top-right? , bottom-right?)
            (True,True)   -> RightZone
            (True,False)  -> UpZone
            (False,True)  -> DownZone
            (False,False) -> LeftZone

spawnClock :: Signal Time
spawnClock = gameClockElapsed --or something. TODO

enemies :: Signal [Enemy]
enemies = foldp3' evolveNmeList [nme0 (10,10),nme0 (15,-50)] gameClock loop (keyDownEvents [SpaceKey])

evolveNmeList :: Time -> Position -> [Key] -> [Enemy] -> [Enemy]
evolveNmeList delta p keys l = fmap (evolveNme delta p) l ++ newNmes
  where
  newNmes = if null keys then [] else [nme0 (100,100)]

evolveNme :: Time -> Position -> Enemy -> Enemy
evolveNme delta p nme = setNmePos nme newpos
  where
    newpos = chaseStep 0.02 20 delta p $ getNmePos nme

getNmePos :: Enemy -> Position
getNmePos (Enemy (Character _ pos)) = pos
setNmePos :: Enemy -> Position  -> Enemy
setNmePos (Enemy (Character c _)) pos = Enemy $ Character c pos
      
nme0 (x,y) = Enemy $ Character 'k' (x,y)


chaseStep :: Double -> Double -> Time -> Position -> Position -> Position
chaseStep speed goalDistance time target start  = 
  if d > goalDistance then closer else start
  where 
    (v1,v2) = target `minus` start
    d = distance start target
    (u1,u2) = (time * v1 / d, time * v2 / d)
    closer = start `plus` (speed * u1, speed * u2)

    
numspaces = countIf (not.null) $ keyDownEvents [SpaceKey]
gameClock = fps 30
gameClockElapsed = foldp (+) 0 gameClock
loop = let r= 50; z = 500 in fmap (\t -> (r * cos (t/z), r * sin (t/z))) gameClockElapsed
--simulation :: H.Signal (SignalGen Engine (Elerea.Signal (Sample Element)))
simulation = GameState <~ pc ~~ enemies

  
main = do
    run config $ render 800 600 <~ simulation
  where
    config = defaultConfig { windowTitle = "Dungeon Dash!", windowPosition = (200,200) }