module Keyboard(defaultKeyDownEvents, keyDownEvents, Key( .. )) where

import FRP.Helm.Keyboard (Key (..), isDown)
import FRP.Helm.Signal (foldp, Signal)
import Data.Traversable (traverse)

defaultKeyDownEvents :: Signal [Key]
defaultKeyDownEvents = keyDownEvents [UpKey,DownKey,LeftKey,RightKey]

debugKeyDownEvents :: Signal [Key]
debugKeyDownEvents = keyDownEvents [SpaceKey]

keyDownEvents :: [Key] -> Signal [Key]
keyDownEvents ks = fmap concat (traverse keyDownEvents1 ks)

keyDownEvents1 :: Key -> Signal [Key]
keyDownEvents1 k = foldp toDown [] (isDown k) 
  where
  -- currently gets marked as a changed sample on both keyup and keydown
  -- for different behaviour we want to use something instead of foldp
    toDown True [] = [k]
    toDown _ _ = []
	