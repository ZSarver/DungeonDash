module Enemy where

import Types
import Vector
import Event (targets)
import Data.List (find)
import FRP.Helm.Color (Color)

modifyBy :: (a -> Bool) -> (a -> a) -> [a] -> [a]
modifyBy _ _ [] = []
modifyBy test f (x:xs) = (if test x then f x else x) : modifyBy test f xs

enemiesInit :: Enemies
enemiesInit = Enemies []

swap (a,b) = (b,a)

enemiesStep :: Time -> Player -> Events -> Enemies -> Enemies
enemiesStep dt p events Enemies{list=elist} = Enemies 
  $ (\e -> setHighlights ((fmap . fmap) zoneColors (targets p $ Enemies e)) e)
  $ fmap (enemyStep dt p) 
  $ filter (not.dead) 
  $ foldr handleEvent elist events
  where
  handleEvent :: Event -> [Enemy] -> [Enemy]
  handleEvent event enemyList = case event of
    HitEnemy nme    -> modifyBy (==nme) hit enemyList
    AttackEnemy nme -> modifyBy (==nme) stun enemyList
    SpawnEnemy here -> insertEnemy (newEnemy here) enemyList
    _               -> enemyList

setHighlights :: [(Enemy, Color)] -> [Enemy] -> [Enemy]
setHighlights highlights es = fmap f es
  where
  f e = case find ((==e).fst) highlights of
          Nothing    -> e{highlight = Nothing}
          Just (_,c) -> e{highlight = Just c}

insertEnemy :: (EnemyID -> Enemy) -> [Enemy] -> [Enemy]
insertEnemy f list = insertEnemyAux 0 list
  where
  insertEnemyAux :: Int -> [Enemy] -> [Enemy]
  insertEnemyAux n [] = [f n]
  insertEnemyAux n (e@Enemy{eid=id}:rest) = if id > n 
    then f n : e : rest 
    else e : insertEnemyAux (n + 1) rest

dead :: Enemy -> Bool
dead Enemy{estate = x} = x == Dead
    
hit :: Enemy -> Enemy
hit e = e{estate = Dead}

stun :: Enemy -> Enemy
stun e = e{estate = Stunned}

enemyStep :: Time -> Player -> Enemy -> Enemy
enemyStep dt Player{ppos=p} e@Enemy{epos=oldpos} = e{epos=newpos}
  where 
  speed = 0.08
  space = 30
  newpos = chaseStep speed space dt p oldpos

newEnemy pos eid = Enemy 'k' pos Alive eid Nothing

chaseStep :: Double -> Double -> Time -> Position -> Position -> Position
chaseStep speed goalDistance time target start  = 
  if d > goalDistance then closer else start
  where 
    vec = target `minus` start
    d = distance start target
    closer = start `plus` ((speed * time / d) `scalarMult` vec)
