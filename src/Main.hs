
import Game (newGame, GameState(..))
import Render
import Graphics hiding (render)

import FRP.Helm hiding (Time)
import FRP.Helm.Time(fps)
import FRP.Helm.Sample (Sample)
import FRP.Elerea.Param hiding (Signal)
import Control.Applicative

--helmify :: E.SignalGen Helm.Engine (E.Signal a) -> Signal a
helmify x = Signal $ (fmap.fmap) pure x

main = do
  let game = helmify $ newGame 0.02 12345
      e = fmap enemies game
      p = fmap player game
      s = fmap sfx game
  run config $ refresh $ render 800 600 <~ p ~~ e ~~ s
  where
    config = defaultConfig { windowTitle = "Dungeon Dash!", windowPosition = (200,200) }
    refresh s = const <~ s ~~ (fps 40)
