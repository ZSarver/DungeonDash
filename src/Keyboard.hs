module Keyboard (clockedKeyDownEvents, keyDownEvents, Key( .. )) 
where

import SignalUtils (mask)

import FRP.Helm.Keyboard (Key (..), isDown)
import FRP.Helm.Signal (foldp, Signal (..))
import FRP.Helm.Sample
import Data.Traversable (traverse)
import FRP.Elerea.Param (transfer)
import Control.Applicative

keyDownEvents :: [Key] -> Signal [Key]
keyDownEvents ks = fmap concat (traverse keyDownEvents1 ks)

keyDownEvents1 :: Key -> Signal [Key]
keyDownEvents1 k = Signal $ signalGen (isDown k) >>= transfer (Unchanged []) f
  where 
    f _ (Changed True) _ = Changed [k]
    f _             _  _ = Unchanged []
    
clockedKeyDownEvents :: Signal a -> [Key] -> Signal [Key]
clockedKeyDownEvents s ks = fmap concat $ traverse (clockedKeyDownEvents1 s) ks

clockedKeyDownEvents1 :: Signal a -> Key -> Signal [Key]
clockedKeyDownEvents1 s k = fmap fst $ Signal $ 
  signalGen (mask s $ isDown k) >>= -- b :: E.Signal (Sample Bool)
  transfer (Unchanged ([],False)) f
  where
    f _ down last = let 
      d = value down
      l = snd . value $ last 
      in if d && (not l) 
        then Changed   ([k],d) 
        else Unchanged ( [],d)