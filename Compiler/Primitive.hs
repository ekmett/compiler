{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Compiler.Primitive 
  ( casIntArray 
  ) where

import Control.Monad.Primitive
import Data.Primitive.ByteArray
import GHC.Int
import GHC.Prim

casIntArray :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> Int -> Int -> m Int
casIntArray (MutableByteArray m) (I# i) (I# o) (I# n) = primitive $ \s -> case casIntArray# m i o n s of
  (# s', a #) -> (# s', I# a #)

