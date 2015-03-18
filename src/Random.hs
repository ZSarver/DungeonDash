{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Random (Rng, Rand, mkRng, splitRng, withRng, rand, range ) where
import System.Random --(Random, randomR, randomRIO,mkStdGen,randomIO, StdGen)
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.State.Strict

type Rand a = State StdGen a

-- Use this to feed random values into a function
-- Given f :: Double -> b
-- fmap f $ range (0,1) :: Rand b
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

feed :: StdGen -> Rand b -> (b,StdGen)
feed g f = runState f g

data Rng = Rng (TVar StdGen)
--This is a pointer to our prng. Using TVar so that if the FRP library
-- tries to pull random numbers in parallel we won't repeat numbers.
-- The STM library will force one of them to retry using the updated prng state.
mkRng :: Int -> IO Rng
mkRng i = atomically . fmap Rng . newTVar $ mkStdGen i

splitRng :: Rng -> IO Rng
splitRng (Rng gen) = atomically $ do
  (g1,g2) <- fmap split $ readTVar gen
  writeTVar gen g1
  fmap Rng $ newTVar g2

withRng :: Rng -> Rand b -> IO b
withRng (Rng gen) f = atomically $ do
    g <- readTVar gen
    let (result, g') = feed g f
    writeTVar gen g'
    return result

                      
