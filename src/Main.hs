import Keyboard (defaultKeyDownEvents, keyDownEvents, Key( .. ))
import Types
import Render
import SignalUtils

import FRP.Helm
import Control.Applicative
import FRP.Helm.Time(fps)
import FRP.Helm.Random (float)
import Data.Traversable (sequenceA)

gameClock = fps 30
gameClockElapsed = foldp (+) 0 gameClock

pc :: Signal Character
pc = Character '@' <~ loop
--pc = pure $ Character '@' (0,0)
loop = let r= 50; z = 500 in fmap (\t -> (r * cos (t/z), r * sin (t/z))) gameClockElapsed

enemy1 :: Signal Enemy
enemy1 = fmap Enemy $ Character 'k' <~ chase 3 60 (60,60) (fmap position pc)

enemy2 = fmap Enemy $ Character 'k' <~ chase 4 20 (0,60) (fmap position pc)

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

simulation :: Signal GameState
simulation = GameState <~ pc ~~ sequenceA [enemy1,enemy2] ~~ spawnClock

mainElement :: Signal Element
mainElement = render 800 600 <~ simulation

main = do
    run config $ mainElement
  where
    config = defaultConfig { windowTitle = "Dungeon Dash!", windowPosition = (200,200) }