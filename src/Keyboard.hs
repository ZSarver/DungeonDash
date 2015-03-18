module Keyboard ( keyDownEvents, Key( .. )) 
where

import FRP.Helm.Keyboard (Key (..))
import FRP.Elerea.Param
import Data.List (elemIndices)
import Foreign (alloca, peekArray, peek, Word8, Ptr)
import Foreign.C.Types

import Control.Applicative
import Data.Traversable (sequenceA)

foreign import ccall unsafe "SDL_GetKeyboardState" sdlGetKeyState :: Ptr CInt -> IO (Ptr Word8)

getKBState :: IO [Int]
getKBState = alloca $ \numkeysPtr -> do
  keysPtr <- sdlGetKeyState numkeysPtr
  numkeys <- peek numkeysPtr
  (map fromIntegral . elemIndices 1) <$> peekArray (fromIntegral numkeys) keysPtr

isDown :: Key -> SignalGen p (Signal Bool)
isDown k = effectful $ elem (fromEnum k) <$> getKBState

keyDownEvents1 :: Key -> SignalGen p (Signal [Key])
keyDownEvents1 k = (liftA2.liftA2) f (isDown k) (wasUp k)
  where
  f True True = [k]
  f _ _ = []
  wasUp k = isDown k >>= delay True . fmap not

keyDownEvents :: [Key] -> SignalGen p (Signal [Key])
keyDownEvents ks = do
  k <- sequenceA $ fmap keyDownEvents1 ks
  return $ fmap concat $ sequenceA k