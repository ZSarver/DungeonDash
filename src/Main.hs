
import Game (newGame, GameState(..))
import Render

import FRP.Helm hiding (Time)
import FRP.Helm.Time(fps)
import FRP.Helm.Sample (Sample)
import FRP.Elerea.Param hiding (Signal)
import qualified FRP.Elerea.Param as E
import System.Random (Random, randomRIO,mkStdGen,randomIO)
import Control.Applicative
import Control.Monad.IO.Class (liftIO)

--helmify :: E.SignalGen Helm.Engine (E.Signal a) -> Signal a
helmify x = Signal $ (fmap.fmap) pure x

main = do
  let game = helmify $ newGame (0.01) 12345
      e = refresh $ fmap enemies game
      p = refresh $ fmap player game
  run config $ render 800 600 <~ p ~~ e
  where
    config = defaultConfig { windowTitle = "Dungeon Dash!", windowPosition = (200,200) }
    refresh s = const <~ s ~~ (fps 40)
