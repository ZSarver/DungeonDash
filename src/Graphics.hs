--Graphics.hs
module Graphics where

import Types
import Vector
import Graphics.UI.SDL.Types hiding (Texture)
import qualified Graphics.UI.SDL.Types as SDL
import Graphics.UI.SDL.Enum (RendererFlip)
import Graphics.UI.SDL.Video (renderClear, renderCopyEx, renderPresent, loadBMP, createTextureFromSurface)
import Foreign.Ptr (Ptr(..))
import Foreign.Marshal.Utils (with)
import Foreign.C.Types
import Foreign.C.String (withCAString)
import Foreign.Storable (peek)
import Control.Monad (sequence)
import Control.Monad.Error (ErrorT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar
import Data.Map
import Prelude hiding (lookup)

{-data RenderObject = RenderObject { image :: Image
                                 , srcrect :: Rect
                                 , dstrect :: Rect
                                 , rotationAngle :: Double
                                 , rotationCenter :: Position
                                 , flip :: RendererFlip
                                 }-}

data RenderObject = RenderObject { robjectImage ::Image, robjectPosition :: Position }
                                 
data Texture = Texture { textureRect :: !Rect, textureData :: !SDL.Texture}

type TextureStore = MVar (Map Image Texture)

vecToPoint :: Vec2 -> Point
vecToPoint (Vec2 x y) = Point (round x) (round y)

centerAt :: Position -> Rect -> Rect
centerAt (Vec2 x y) (Rect _ _ w h) = Rect x' y' w h
  where
  x' = round x + (w `div` 2)
  y' = round y + (h `div` 2)

getImage :: Renderer -> TextureStore -> Image -> IO Texture
getImage r assets i = do
  assets' <- takeMVar assets
  let mv = lookup i assets'
  case mv of
    Just tex -> do
      putMVar assets assets'
      return tex
    Nothing -> do
      s <- withCAString i loadBMP
      tex' <- createTextureFromSurface r s
      texr <- fmap (\a -> Rect 0 0 (surfaceW a) (surfaceH a)) $ peek s
      let tex = Texture {textureData = tex', textureRect = texr} 
      putMVar assets $ insert i tex assets'
      return tex

drawToScreen :: Renderer -> TextureStore -> [RenderObject] -> ErrorT String IO ()
drawToScreen r texstore robjects = do
  liftIO $ renderClear r
  mapM_ render' robjects :: ErrorT String IO () -- Left String or Right [()]
  liftIO $ renderPresent r
  return ()
  where
    render' :: RenderObject -> ErrorT String IO ()
    render' RenderObject{ .. } = do
      texture <- liftIO $ getImage r texstore robjectImage
      let srcrect = textureRect texture
          dstrect = centerAt robjectPosition srcrect
          flip = 0
      errno <- liftIO $ 
        with srcrect $ \srcrect' -> 
        with dstrect $ \dstrect' -> 
        with (Point 0 0) $ \center' ->
        let angle' = 0
        in  renderCopyEx r (textureData texture) srcrect' dstrect' angle' center' flip
      if errno == 0
        then return ()
        else throwError $ "Error in SDL.renderCopyEx: " ++ (show errno) 