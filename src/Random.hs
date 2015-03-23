module Random (Rng, Rand, rand, range, filterRand,mkStdGen ) where
import System.Random --(Random, randomR, randomRIO,mkStdGen,randomIO, StdGen)
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.State.Strict

type Rand a = State StdGen a

range :: Random a => (a,a) -> Rand a
range r = do
  (x,g') <- fmap (randomR r) get
  put g'
  return x

rand :: Random a => Rand a
rand = do
  (x,g') <- fmap random get
  put g'
  return x

filterRand :: (a -> Bool) -> Rand a -> Rand a
filterRand f r = do
  (x,g') <- fmap (runState r) get
  put g'
  if f x then return x else filterRand f r
  
type Rng = StdGen
