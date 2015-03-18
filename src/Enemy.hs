module Enemy where

import Types
import Vector

modifyBy :: (a -> Bool) -> (a -> a) -> [a] -> [a]
modifyBy _ _ [] = []
modifyBy test f (x:xs) = (if test x then f x else x) : modifyBy test f xs

enemiesInit :: Enemies
enemiesInit = Enemies []
enemiesStep :: Time -> Player -> Events -> Enemies -> Enemies
enemiesStep dt p events Enemies{list=elist} = Enemies 
  $ fmap (enemyStep dt p) 
  $ filter (not.dead) 
  $ foldr handleEvent elist events
  where
  handleEvent :: Event -> [Enemy] -> [Enemy]
  handleEvent event enemyList = case event of
    HitEnemy e      -> modifyBy (==e) hit enemyList
    SpawnEnemy here -> insertEnemy (Enemy 'k' here Alive) enemyList
    _               -> enemyList


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

enemyStep :: Time -> Player -> Enemy -> Enemy
enemyStep dt Player{ppos=p} e@Enemy{epos=oldpos} = e{epos=newpos}
  where 
  speed = 0.04
  space = 30
  newpos = chaseStep speed space dt p oldpos

nme0 v = Enemy 'k' v Alive 0


chaseStep :: Double -> Double -> Time -> Position -> Position -> Position
chaseStep speed goalDistance time target start  = 
  if d > goalDistance then closer else start
  where 
    vec = target `minus` start
    d = distance start target
    closer = start `plus` ((speed * time / d) `scalarMult` vec)
