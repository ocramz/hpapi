------------------------------------------------------------------------
-- |
-- Module   : System.PAPI.Util
-- Copyright: Copyright (c) 2008, Michael D. Adams
-- License  : BSD3
--
-- Maintainer:  Michael D. Adams <adamsmd [AT] cs.indiana.edu>
-- Stability :  alpha
--
------------------------------------------------------------------------
--
-- Binding for the PAPI library: private helper functions.
-- Users should not import
--

module System.PAPI.Util where

import Foreign
import Foreign.C

data Void
type Addr = Ptr Void -- Equivalent of a void*

infixl 0 `ap2`
ap2 :: (Monad m) => (x -> m (a -> b)) -> (x -> m a) -> (x -> m b)
ap2 f g = \x -> do
  f' <- f x
  g' <- g x
  return (f' g')

pokeCString :: CString -> String -> Int -> IO ()
pokeCString dst value maxLen =
  withCStringLen (take maxLen value) $ uncurry (copyArray dst)

nullThrow :: String -> Ptr a -> IO (Ptr a)
nullThrow str ptr = if ptr == nullPtr then error str else return ptr

-- Casting functions

toMask :: (Num a) => Bool -> a -> a
toMask bool mask = if bool then mask else 0

toCLLong :: Integer -> CLLong
toCLLong x = fromInteger x

fromCLLong :: CLLong -> Integer
fromCLLong x = toInteger x

toCInt :: Integer -> CInt
toCInt x = fromInteger x

fromCInt :: CInt -> Integer
fromCInt x = toInteger x

toCUInt :: Integer -> CUInt
toCUInt x = fromInteger x

fromCUInt :: CUInt -> Integer
fromCUInt x = toInteger x

wordToBool :: Word -> Bool
wordToBool = (/=0)

boolToWord :: Bool -> Word
boolToWord x = if x then 1 else 0
