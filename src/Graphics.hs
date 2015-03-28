--Graphics.hs
module Graphics where

import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Video (renderClear, renderCopy, renderPresent)
import Foreign.Ptr (Ptr(..))
import Foreign.C.Types
import Control.Monad (sequence)


untriple :: (a,b,c) -> ((a,b),c)
untriple (x,y,z) = ((x,y),z)

render :: Renderer -> [(Texture, Ptr Rect, Ptr Rect)] -> IO [CInt]
render r ts = do
  renderClear r
  let rc = renderCopy r
  let ts' = map untriple ts
  let res = map (uncurry (uncurry rc)) ts' :: [IO CInt]
  renderPresent r
  return $ sequence res