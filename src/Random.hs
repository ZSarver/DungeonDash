{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
module Random (Rng, mkRng, withGen, randomly, randomly2, randomly3) where
import System.Random --(Random, randomR, randomRIO,mkStdGen,randomIO, StdGen)
import FRP.Elerea.Param
import Control.Concurrent.STM

data WantsRandom a b = R (a -> b) (a,a) 
data Makes b = forall a. (Random a) => More (WantsRandom a (Makes b)) | Done b
--"Makes b" is the type for functions that will eventually produce a b
-- after receiving some random numbers. The wacky type is to be able to
-- deal with functions that want many random numbers

data Rng = Rng (TVar StdGen)
--This is a pointer to our prng. Using TVar so that if the FRP library
-- tries to pull random numbers in parallel we won't repeat numbers.
-- The STM library will force one of them to retry using the updated prng state.

mkRng :: Int -> IO Rng
mkRng i = atomically . fmap Rng . newTVar $ mkStdGen i

--withGen and randomly are going to be our primary ways of introdocuing random numbers
-- Given f :: Signal (Double -> b) then
-- randomly <$> pure (1,2) <*> f :: Signal (Makes b)
-- and if we also have g :: Rng, then
-- withGen g $ randomly <$> pure (1,2) <*> f :: SignalGen p (Signal b)
withGen :: Rng -> Signal (Makes b) -> SignalGen p (Signal b)
withGen (Rng gen) s = effectful1 eval s
  where
  eval f = atomically $ do
    g <- readTVar gen
    let (result, g') = feed f g
    writeTVar gen g'
    return result
   
feed :: Makes b -> StdGen -> (b,StdGen)
feed f g = case f of
   (More (R f' range)) -> let (x, g') = randomR range g in feed (f' x) g'
   (Done x)            -> (x, g)

randomly :: Random a => (a,a) -> (a -> b) -> Makes b
randomly r f = More (R (Done . f) r)

randomly2 :: (Random a1, Random a2) => (a1,a1) -> (a2,a2) -> (a1 -> a2 -> b) -> Makes b
randomly2 r1 r2 f = More (R (randomly r2 . f) r1)

randomly3 :: (Random a1, Random a2, Random a3) => 
  (a1,a1) -> (a2,a2) -> (a3,a3) ->
  (a1 -> a2 -> a3 -> b) -> Makes b
randomly3 r1 r2 r3 f = More (R (randomly2 r2 r3 . f) r1)

   