{-# LANGUAGE RecordWildCards #-}
module Types where

import FRP.Helm.Keyboard (Key)
import FRP.Helm (Time)

data Enemy = Enemy {chr :: Character}
data Zone = UpZone | DownZone | LeftZone | RightZone | OutZone

type Position = (Double,Double)

data Character = Character 
  { char :: Char
  , position :: Position
  }
  
data Inputs = Inputs
  { delta :: Time
  , randTick :: Float
  , kbin :: [Key]
  }

data GameState = GameState
  { player :: Character
  , enemies :: [Enemy]
  , spawnTimer :: !Time
  }
  
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
