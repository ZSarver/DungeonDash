module Types where

import Vector
import Keyboard (Key(..))
import FRP.Helm.Color (rgba, Color, yellow, red, blue, green)

type Time = Double

data Event = HitEnemy Enemy
           | SpawnEnemy Position
           | AttackEnemy Enemy
type Events = [Event]

type EnemyID = Int
data EnemyState = Alive | Dead | Stunned deriving (Eq)
data Enemy = Enemy { echar     :: Char
                   , epos      :: Position
                   , estate    :: EnemyState
                   , eid       :: EnemyID
                   , highlight :: Maybe Color
                   }
instance Eq Enemy where
  e == f = eid e == eid f
data Enemies = Enemies {list :: [Enemy]}

data PlayerAction = Waiting | Flying { flyFrom :: Position
                                     , flyTo :: Enemy
                                     , flyDuration :: Time
                                     , flyElapsed :: Time} deriving (Eq)

data Player = Player {pchar :: Char, ppos :: Position, zoneRadius :: Double, pact :: PlayerAction}

data Zone = UpZone | DownZone | LeftZone | RightZone | OutZone deriving (Eq, Enum)

zoneColors z = colorlist !! fromEnum z

colorlist = [yellow, green, blue, red, (rgba 0 0 0 0)]

lerp a b t = (1-t)*a + t*b
{-
class StepWith a z where
  stepWith :: a -> z -> z
class StepWith2 a b z where
  stepWith2 :: a -> b -> z -> z
class StepWith3 a b c z where
  stepWith3 :: a -> b -> c -> z -> z
instance (StepWith a z, StepWith b z) => StepWith2 a b z where
  stepWith2 a b = stepWith b . stepWith a
instance (StepWith2 a b z, StepWith c z) => StepWith3 a b c z where
  stepWith3 a b c = stepWith c . stepWith2 a b

class Update a where
  update :: Inputs -> a -> a

instance Update Enemy where
  update i e = e

instance Update Double where
  update Inputs{ .. } d = let x = d + delta in seq x x
  
instance Update GameState where  
  update i g@GameState{ .. } = g
    { enemies = fmap (update i) enemies
    , spawnTimer = update i spawnTimer
    }
-}