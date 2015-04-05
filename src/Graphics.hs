--Graphics.hs
module Graphics where

import Types
import Vector
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Enum (RendererFlip)
import Graphics.UI.SDL.Video (renderClear, renderCopyEx, renderPresent)
import Foreign.Ptr (Ptr(..))
import Foreign.Marshal.Utils (with)
import Foreign.C.Types
import Control.Monad (sequence)
import Control.Monad.Error (ErrorT (..), throwError)
import Control.Monad.IO.Class (liftIO)

data RenderObject = RenderObject { texture :: Texture
                                 , srcrect :: Rect
                                 , dstrect :: Rect
                                 , angle :: Double
                                 , center :: Position
                                 , flip :: RendererFlip
                                 }
{-                                                                  
untriple :: (a,b,c) -> ((a,b),c)
untriple (x,y,z) = ((x,y),z)

renderTriple :: RenderObject -> ((Texture, Ptr Rect), Ptr Rect)
renderTriple ro = ((texture ro, source ro), dest ro)
-}
vecToPoint :: Vec2 -> Point
vecToPoint (Vec2 x y) = Point (round x) (round y)

render :: Renderer -> [RenderObject] -> ErrorT String IO ()
render r ts = do
  liftIO $ renderClear r
  -- let ts' = map renderTriple ts    
  mapM_ render' ts :: ErrorT String IO () -- Left String or Right [()]
  liftIO $ renderPresent r
  return ()
  where
    render' :: RenderObject -> ErrorT String IO ()
    render' RenderObject{ .. } = do
      errno <- liftIO $ 
        with srcrect $ \srcrect' -> 
        with dstrect $ \dstrect' -> 
        with (vecToPoint center) $ \center' ->
        let angle' = realToFrac angle
        in  renderCopyEx r texture srcrect' dstrect' angle' center' flip
      if errno == 0
        then return ()
        else throwError $ "Error in SDL.renderCopyEx: " ++ (show errno) 