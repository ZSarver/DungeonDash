--Graphics.hs
module Graphics where

import Types
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Video (renderClear, renderCopy, renderPresent)
import Foreign.Ptr (Ptr(..))
import Foreign.C.Types
import Control.Monad (sequence)


untriple :: (a,b,c) -> ((a,b),c)
untriple (x,y,z) = ((x,y),z)

renderTriple :: RenderObject -> ((Texture, Ptr Rect), Ptr Rect)
renderTriple ro = ((texture ro, source ro), dest ro)

render :: Renderer -> [RenderObject] -> IO [CInt]
render r ts = do
  renderClear r
  let ts' = map renderTriple ts    
  let res = map (uncurry (uncurry (renderCopy r))) ts' :: [IO CInt]
  renderPresent r
  (sequence res) >>= return