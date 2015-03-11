{-# LANGUAGE RecordWildCards #-}
import FRP.Helm
import Control.Applicative
import FRP.Helm.Text (text, color, toText)
import FRP.Helm.Color
import FRP.Helm.Time(fps)
import FRP.Helm.Random (float)

render :: Int -> Int -> GameState -> Element
render w' h' g = centeredCollage w' h' (background : drawCharacter red p : fmap (drawCharacter green) e)
  where
  GameState p e _ = g
  (w,h) = (fromIntegral w', fromIntegral h')
  drawSymbol a c = toForm . text . color c . toText $ [a]
  drawCharacter col c = let (Character a x y) = toCharacter c in move (x,y) (drawSymbol a col)
  background =  filled black $ rect 800 600
  
  
data Character = Character 
  { c :: Char
  , x :: Double
  , y :: Double
  }

data Enemy = Enemy {chr :: Character}

class ToCharacter a where
  toCharacter :: a -> Character
  
instance ToCharacter Character where
  toCharacter = id
  
instance ToCharacter Enemy where
  toCharacter = chr

class Update a where
  update :: Inputs -> a -> a
 
instance Update Enemy where
  update i e = e

instance Update Double where
  update Inputs{ .. } d = let x = d + delta in seq x x
  
data Inputs = Inputs
  { delta :: Time
  , randTick :: Float
  , kbin :: [Key]
  }
data Key = Key
  
data GameState = GameState
  { player :: Character
  , enemies :: [Enemy]
  , spawnTimer :: !Time
  }

instance Update GameState where  
  update i g@GameState{ .. } = g
    { enemies = fmap (update i) enemies
    , spawnTimer = update i spawnTimer
    }

simulation :: Signal GameState
simulation = foldp update initialState i
  where 
    i = Inputs <~ ticks ~~ float ticks ~~ pure []
    ticks = fps 30
    
initialState = GameState (Character '@' 0 0) [Enemy $ Character 'k' 30 30] 0

mainElement :: Signal Element
mainElement = render 800 600 <~ (pure initialState)

main = do
    run config $ mainElement
  where
    config = defaultConfig { windowTitle = "Helm - Mode Switching", windowPosition = (200,200) }