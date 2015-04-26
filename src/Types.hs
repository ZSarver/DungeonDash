{-# LANGUAGE DeriveGeneric #-}
module Types where

import Vector
import Keyboard (Key(..))
--import FRP.Helm.Color (rgba, Color, yellow, red, blue, green)
import Graphics.UI.SDL.Types (Texture (..), Rect (..))
import Foreign.Ptr (Ptr(..))
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
--
data Color = Color deriving Generic
instance Serialize Color
--


type Time = Double
type Image = FilePath

data Event = HitEnemy !Enemy
           | SpawnEnemy !Position
           | AttackEnemy !Enemy
type Events = [Event]

type EnemyID = Int
data EnemyState = Alive | Dead | Stunned deriving (Eq, Generic)
instance Serialize EnemyState
data Enemy = Enemy { eimg      :: !Image
                   , epos      :: !Position
                   , estate    :: !EnemyState
                   , eid       :: !EnemyID
                   , highlight :: !(Maybe Color)
                   } deriving (Generic)
instance Serialize Enemy
instance Eq Enemy where
  e == f = eid e == eid f
type Enemies = [Enemy]

data PlayerAction = Waiting | Flying { flyFrom     :: !Position
                                     , flyTo       :: !Enemy
                                     , flyDuration :: !Time
                                     , flyElapsed  :: !Time} deriving (Eq, Generic)
instance Serialize PlayerAction
data Player = Player { pimg       :: !Image
                     , ppos       :: !Position
                     , zoneRadius :: !Double
                     , pact       :: !PlayerAction
                     } deriving (Generic)
instance Serialize Player
data Zone = UpZone | DownZone | LeftZone | RightZone | OutZone deriving (Eq, Enum)