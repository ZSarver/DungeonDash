import Keyboard (defaultKeyDownEvents, keyDownEvents, Key( .. ))
import Types
import Render

import FRP.Helm
import Control.Applicative
import FRP.Helm.Time(fps)
import FRP.Helm.Random (float)

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