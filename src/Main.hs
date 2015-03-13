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
import FRP.Elerea.Param (effectful, transfer2, transfer,snapshot)
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

--foldp ::  (a -> b -> b) -> b -> Signal a -> Signal b
enemies :: Signal [Enemy]
enemies = sequenceA [enemy1,enemy2]
{-enemies = fmap . sequenceA $ foldp f [] debugKeyDownEvents
  where
    f :: [Key] -> [Signal Enemy] -> [Signal Enemy]
    f keylist enemylist = if SpaceKey `elem` keylist
                          then undefined
                          else enemylist
-}

--simulationInit = return $ GameState <~ pc ~~ enemies
{-
    step time to@(t1,t2) from@(f1,f2) = 
      let (v1,v2) = to `minus` from
          d = distance from to
          (u1,u2) = (v1 / d, v2 / d)
          closer = from `plus` (speed * u1, speed * u2)
      in  if d > goalDistance then closer else from-}
getNmePos :: Enemy -> Position
getNmePos (Enemy (Character _ pos)) = pos
setNmePos :: Enemy -> Position  -> Enemy
setNmePos (Enemy (Character c _)) pos = Enemy $ Character c pos
      
nme0 (x,y) = (pure $ Enemy $ Character 'k' (x,y)) :: Sample Enemy

evolveNme :: a -> Sample Position -> Sample Time -> Sample Enemy -> Sample Enemy
evolveNme _ p delta nme = setNmePos <$> nme <*> newpos
  where
    newpos = chaseStep <$> pure 0.02 <*> pure 20 <*> delta <*> p <*> fmap getNmePos nme

chaseStep :: Double -> Double -> Time -> Position -> Position -> Position
chaseStep speed goalDistance time target start  = 
  if d > goalDistance then closer else start
  where 
    (v1,v2) = target `minus` start
    d = distance start target
    (u1,u2) = (time * v1 / d, time * v2 / d)
    closer = start `plus` (speed * u1, speed * u2)

gameClock = fps 30
gameClockElapsed = foldp (+) 0 gameClock
loop = let r= 50; z = 500 in fmap (\t -> (r * cos (t/z), r * sin (t/z))) gameClockElapsed
--simulation :: IO ( H.Signal (SignalGen Engine (Elerea.Signal (Sample Element)))
simulation = Signal $ do
  pos <- signalGen loop
  dt <- signalGen gameClock
  let player = (liftA2.liftA2) Character ((pure.pure) '@') pos
  -- nme :: Position -> SignalGen e (Signal (Sample Enemy))
  let nme (x,y) = transfer2 (nme0 (x,y)) evolveNme pos dt 
  nmes <- sequence [nme (-20, 30)]
  --let nmes' = fmap sequenceA . sequenceA $ nmes
  spaceDown <- signalGen $ fmap (not.null) $ keyDownEvents [SpaceKey]
  --  spawn :: a -> Sample Bool 
  --   -> [SignalGen e (Signal (Sample Enemy))] 
  --   -> [SignalGen e (Signal (Sample Enemy))]
  let spawn _ b oldlist = case value b of 
        True  -> (nme (30,60)):oldlist
        False -> oldlist
  -- nmeGen :: Signal [SignalGen e (Signal (Sample Enemy))]
  nmeGen <- transfer [nme (30,60)] spawn spaceDown
  -- nmeGen' :: [SignalGen e (Signal (Sample Enemy))]
  nmeGen' <- snapshot nmeGen
  -- nmeGen' :: [(Signal (Sample Enemy))]
  nmeGen'' <- sequence nmeGen'
  let nmes' = fmap sequenceA . sequenceA $ nmeGen''
  return $ (liftA2.liftA2) GameState player nmes' -- :: Elerea.Signal (Sample GameState)
  
  
main = do
    run config $ render 800 600 <~ simulation
  where
    config = defaultConfig { windowTitle = "Dungeon Dash!", windowPosition = (200,200) }