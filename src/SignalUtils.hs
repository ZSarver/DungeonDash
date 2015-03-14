module SignalUtils where
--This is stuff that helm should prolly provide
--we can pick through it later for things to give them

import FRP.Helm
import FRP.Helm.Sample (Sample(..), value)
import FRP.Elerea.Param (transfer2)
import Control.Applicative

--This belongs in helm.sample
--suppresses the changed token in a sample
quiet :: Sample a -> Sample a
quiet = Unchanged . value


--These belong in helm.signal
--suppresses all changed tokens in a signal
silence :: Signal a -> Signal a
silence (Signal x) = Signal $ (fmap.fmap) quiet x

--Ignores all changes in the second signal until the first signal updates
mask :: Signal a -> Signal b -> Signal b
mask x y = maybe <~ (silence y) ~~ (pure id) ~~ yy
  where
    yy = maskAux x y
unmask :: Signal a -> Signal b -> Signal b
unmask = liftA2 $ flip const

maskAux :: Signal a -> Signal b -> Signal (Maybe b)
maskAux (Signal x') (Signal y') = Signal $ do
  x <- x'
  y <- y'
  transfer2 (Unchanged Nothing) f x y
  where
    f _ x@(Changed _) y _ = const <$> fmap Just y <*> x
    f _ _             _ o = quiet o

--foldp#' behaves like foldp for multiple signals
--except that the updates of input signals after the first one
--are masked by the first one.
foldp2' :: (s -> t1 -> a -> a) -> a -> Signal s -> Signal t1 -> Signal a
foldp2' f init s t = foldp f' init s'
  where
    s' = mask s $ lift2 (,) s t
    f' (x1,x2) z = f x1 x2 z
foldp3' :: (s -> t1 -> t2 -> a -> a) -> a -> Signal s -> Signal t1 -> Signal t2 -> Signal a
foldp3' f init s t1 t2 = foldp f' init s'
  where
    s' = mask s $ lift3 (,,) s t1 t2
    f' (x1,x2,x3) z = f x1 x2 x3 z
foldp4' :: (s -> t1 -> t2 -> t3 -> a -> a)
  -> a -> Signal s 
  -> Signal t1 -> Signal t2 -> Signal t3 
  -> Signal a
foldp4' f init s t1 t2 t3 = foldp f' init s'
  where
    s' = mask s $ lift4 (,,,) s t1 t2 t3
    f' (x1,x2,x3,x4) z = f x1 x2 x3 x4 z
foldp5' :: (s -> t1 -> t2 -> t3 -> t4 -> a -> a)
  -> a -> Signal s 
  -> Signal t1 -> Signal t2 -> Signal t3 -> Signal t4
  -> Signal a
foldp5' f init s t1 t2 t3 t4 = foldp f' init s'
  where
    s' = mask s $ lift5 (,,,,) s t1 t2 t3 t4
    f' (x1,x2,x3,x4,x5) z = f x1 x2 x3 x4 x5 z
    
