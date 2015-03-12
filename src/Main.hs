import Keyboard (defaultKeyDownEvents, keyDownEvents, Key( .. ))
import Types
import Render
import SignalUtils

import FRP.Helm
import Control.Applicative
import FRP.Helm.Time(fps)
import FRP.Helm.Random (float)

gameClock = fps 30

simulation :: Signal GameState
simulation = foldp update initialState i
  where 
    i = Inputs <~ gameClock ~~ float gameClock ~~ pure []
    
initialState = GameState (Character '@' (0,0)) [Enemy $ Character 'k' (30,30)] 0

mainElement :: Signal Element
mainElement = render 800 600 <~ (pure initialState)

main = do
    run config $ mainElement
  where
    config = defaultConfig { windowTitle = "Helm - Mode Switching", windowPosition = (200,200) }