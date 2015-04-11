
import Game (newGame, GameState(..))
import Render
import Graphics
import qualified Graphics.UI.SDL as SDL
import Data.Bits ((.|.))
import Control.Auto (arrM, Auto (..))
import Control.Auto.Run (run)
import Control.Concurrent.MVar
import Foreign.C (withCAString)
import Data.Map (fromList, Map(..))
{-main = do
  let game = helmify $ newGame 0.02 12345
      e = fmap enemies game
      p = fmap player game
      s = fmap sfx game
  run config $ refresh $ render 800 600 <~ p ~~ e ~~ s
  where
    config = defaultConfig { windowTitle = "Dungeon Dash!", windowPosition = (200,200) }
    refresh s = const <~ s ~~ (fps 40)
-}

gameloop :: Auto IO () ()
gameloop = arrM undefined
  
checkQuit :: () -> IO (Maybe ())
checkQuit _ = fmap f SDL.quitRequested
  where 
  f True = Nothing
  f False = Just ()
  
main = withCAString "Dungeon Dash!" $ \title -> do
  window <- SDL.createWindow title 100 100 800 600 wflags
  renderer <- SDL.createRenderer window (-1) rflags
  texstore <- newMVar (fromList [] :: Map String Texture)
  
  run (return ()) checkQuit gameloop
  where
  wflags = foldl (.|.) 0 $ [SDL.SDL_WINDOW_SHOWN] ++
                           [SDL.SDL_WINDOW_RESIZABLE]
  rflags = (.|.) SDL.SDL_RENDERER_PRESENTVSYNC SDL.SDL_RENDERER_ACCELERATED