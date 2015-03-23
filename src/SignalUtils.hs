module SignalUtils where

import Random

import FRP.Elerea.Param
import Control.Applicative
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.Fixed (mod')
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Control.Monad.State.Strict


accumMaybes :: a -> Signal (Maybe a) -> SignalGen p (Signal a)
accumMaybes x s = transfer x f s
  where f _ new old = fromMaybe old new

-- This is true after t passes in SYSTEM TIME.
timeTicks :: NominalDiffTime -> SignalGen p (Signal Bool)
timeTicks t = do
  start <- liftIO getCurrentTime
  now <- effectful getCurrentTime
  prev <- delay start now
  (fmap.fmap) fst $ transfer (False,0) f (diffUTCTime <$> now <*> prev)
  where
  f _ delta (_,acc) = let acc' = delta + acc in
    if acc' > t then (True,acc' `mod'` t) else (False,acc')

-- This is true after t passes in SUBJECTIVE SIM TIME
-- It doesn't care about units as long as all appearances
-- of 'a' in the signature are in the same unit.
every :: Real a => a -> SignalGen a (Signal Bool)
every t = (fmap.fmap) snd $ stateful (0,False) f
  where 
  f dt (acc,_) = let acc' = dt + acc in 
    if acc' > t then (acc' - t, True) else (acc', False)

evalRandomSignal :: Rng -> Signal (Rand a) -> SignalGen p (Signal a)
evalRandomSignal g s = (fmap.fmap) fst $ transfer (undefined,g) f s
  where
  f _ r (_,gen) = runState r gen

elapsedGameTime = stateful 0 (+)

