{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import Vector

import FRP.Helm.Keyboard (Key)

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

-- a -> z -> z
-- 

type Time = Double

data Enemy = Enemy {chr :: Character}
data Zone = UpZone | DownZone | LeftZone | RightZone | OutZone

data Character = Character 
  { char :: Char
  , position :: Position
  }
  
data Inputs = Inputs
  { delta :: Time
  , randTick :: Float
  , kbin :: [Key]
  }

data GameState = GameState Character [Enemy]
  
{-class Update a where
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