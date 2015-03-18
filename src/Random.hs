{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
module Random (Rng, Rand, mkRng, withRng, rand, range ) where
import System.Random --(Random, randomR, randomRIO,mkStdGen,randomIO, StdGen)
import FRP.Elerea.Param
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad (ap)

data RequestRandom a = W (a,a) | U
data WantsRandom a b = R (a -> b) (RequestRandom a) 
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
range :: Random a => (a,a) -> Rand a
range r = More $ R Done ( W r)

rand :: Random a => Rand a
rand = More $ R Done U

-- To extract stuff out of the Rand type, either 
-- pass a value and a StdGen to feed  or
-- pass a signal and a Rng to withRng 
feed :: StdGen -> Rand b -> (b,StdGen)
feed g f = case f of
   (Done x)        -> (x, g)
   (More (R f' r)) -> let (x, g') = case r of
                                     (W range) -> randomR range g
                                     U         -> random g
                      in feed  g' (f' x)

withRng :: Rng -> Rand b -> IO b
withRng (Rng gen) f = atomically $ do
    g <- readTVar gen
    let (result, g') = feed g f
    writeTVar gen g'
    return result

                      
