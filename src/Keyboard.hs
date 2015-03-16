module Keyboard (defaultKeyDownEvents, keyDownEvents, Key( .. )) 
where

import FRP.Helm.Keyboard (Key (..), isDown)
import FRP.Helm.Signal (foldp, Signal (..))
import FRP.Helm.Sample
import Data.Traversable (traverse)
import FRP.Elerea.Param (transfer)

defaultKeyDownEvents :: Signal [Key]
defaultKeyDownEvents = keyDownEvents [UpKey,DownKey,LeftKey,RightKey]

debugKeyDownEvents :: Signal [Key]
debugKeyDownEvents = keyDownEvents [SpaceKey]

keyDownEvents :: [Key] -> Signal [Key]
keyDownEvents ks = fmap concat (traverse keyDownEvents1 ks)

keyDownEvents1 :: Key -> Signal [Key]
keyDownEvents1 k = Signal $ signalGen (isDown k) >>= transfer (Unchanged []) f
  where 
    f _ (Changed True) _ = Changed [k]
    f _             _  _ = Unchanged []    
