module Types where

import Vector
import Keyboard (Key(..))
--import FRP.Helm.Color (rgba, Color, yellow, red, blue, green)
import Graphics.UI.SDL.Types (Texture (..), Rect (..))
import Foreign.Ptr (Ptr(..))


--
data Color = Color
--


type Time = Double
type Image = FilePath

data Event = HitEnemy !Enemy
           | SpawnEnemy !Position
           | AttackEnemy !Enemy
type Events = [Event]

type EnemyID = Int
data EnemyState = Alive | Dead | Stunned deriving (Eq)
data Enemy = Enemy { eimg      :: !Image
                   , epos      :: !Position
                   , estate    :: !EnemyState
                   , eid       :: !EnemyID
                   , highlight :: !(Maybe Color)
                   }
instance Eq Enemy where
  e == f = eid e == eid f
type Enemies = [Enemy]

data PlayerAction = Waiting | Flying { flyFrom     :: !Position
                                     , flyTo       :: !Enemy
                                     , flyDuration :: !Time
                                     , flyElapsed  :: !Time} deriving (Eq)

data Player = Player { pimg       :: !Image
                     , ppos       :: !Position
                     , zoneRadius :: !Double
                     , pact       :: !PlayerAction
                     }

data Zone = UpZone | DownZone | LeftZone | RightZone | OutZone deriving (Eq, Enum)