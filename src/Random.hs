{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
module Random (Rng, Rand, mkRng, withRng, randR ) where
import System.Random --(Random, randomR, randomRIO,mkStdGen,randomIO, StdGen)
import FRP.Elerea.Param
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad (ap)

data WantsRandom a b = R (a -> b) (a,a) 
data Rand b = forall a. (Random a) => More (WantsRandom a (Rand b)) | Done b
--"Rand b" is the type for functions that can produce a value of type
-- b when fed enough random input.

instance Functor Rand where
  fmap f (Done b) = Done $ f b
  fmap f (More (R g r)) = More $ R (fmap f . g) r

instance Applicative Rand where
  pure = return
  (<*>) = ap

instance Monad Rand where
  return = Done
  (Done a) >>= f = f a
  (More (R x r)) >>= f = More $ R (\t -> x t >>= f) r
  
data Rng = Rng (TVar StdGen)
--This is a pointer to our prng. Using TVar so that if the FRP library
-- tries to pull random numbers in parallel we won't repeat numbers.
-- The STM library will force one of them to retry using the updated prng state.

mkRng :: Int -> IO Rng
mkRng i = atomically . fmap Rng . newTVar $ mkStdGen i

-- Use this to feed random values into a function
-- Given f :: Double -> b
-- fmap f $ randR (0,1) :: Rand b
randR :: Random a => (a,a) -> Rand a
randR r = More $ R Done r

-- To extract stuff out of the Rand type, either 
-- pass a value and a StdGen to feed  or
-- pass a signal and a Rng to withRng 
feed :: StdGen -> Rand b -> (b,StdGen)
feed g f = case f of
   (More (R f' range)) -> let (x, g') = randomR range g in feed  g' (f' x)
   (Done x)            -> (x, g)

withRng :: Rng -> Signal (Rand b) -> SignalGen p (Signal b)
withRng (Rng gen) s = effectful1 eval s
  where
  eval f = atomically $ do
    g <- readTVar gen
    let (result, g') = feed g f
    writeTVar gen g'
    return result
