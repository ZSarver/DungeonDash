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
enemy1 = fmap Enemy $ Character 'k' <~ chase 3 (60,60) (fmap position pc)

chase :: Double -> Position -> Signal Position -> Signal Position
chase speed start target = foldp2' step start gameClock target
  where 
    step time to@(t1,t2) from@(f1,f2) = 
      let (v1,v2) = (t1-f1,t2-f2)
          d = dist from to
          (u1,u2) = (v1 / d, v2 / d)
          closer = (speed * u1 + f1, speed * u2 + f2)
      in  if d > 30 then closer else from

dist :: Position -> Position -> Double
dist (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2
  
spawnClock :: Signal Time
spawnClock = gameClockElapsed --or something. TODO

simulation :: Signal GameState
simulation = GameState <~ pc ~~ sequenceA [enemy1] ~~ spawnClock

mainElement :: Signal Element
mainElement = render 800 600 <~ simulation

main = do
    run config $ mainElement
  where
    config = defaultConfig { windowTitle = "Helm - Mode Switching", windowPosition = (200,200) }