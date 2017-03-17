{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.Disjoint where

import Control.Monad (unless)
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive.MutVar

-- | disjoint set with a monoidal annotation
data Disjoint s a = Disjoint !(MutVar s (Either a (Disjoint s a))) !(MutVar s Int)

-- | NB: this should be used on the result of 'find' rather than in general.
instance Eq (Disjoint s a) where
  Disjoint m _ == Disjoint n _ = m == n

-- find with path-compression
findValue :: forall m a. (PrimMonad m, Monoid a) => Disjoint (PrimState m) a -> m (Disjoint (PrimState m) a, a)
findValue k0 = stToPrim (go k0) where
  go :: Disjoint (PrimState m) a -> ST (PrimState m) (Disjoint (PrimState m) a, a)
  go k@(Disjoint v _) = readMutVar v >>= \case
    Left a -> return (k, a)
    Right k' -> do
      (k'', a) <- findValue k'
      writeMutVar v (Right k'')
      return (k'', a)
  
-- find with path-compression
find :: forall m a. (PrimMonad m, Monoid a) => Disjoint (PrimState m) a -> m (Disjoint (PrimState m) a)
find k0 = stToPrim (go k0) where
  go :: Disjoint (PrimState m) a -> ST (PrimState m) (Disjoint (PrimState m) a)
  go k@(Disjoint v _) = readMutVar v >>= \case
    Left _ -> return k
    Right k' -> do
      k'' <- find k'
      writeMutVar v (Right k'')
      return k''

-- find with path-compression
value :: (PrimMonad m, Monoid a) => Disjoint (PrimState m) a -> m a
value k = stToPrim $ snd <$> findValue k

-- union-by-rank
union :: (PrimMonad m, Monoid a) => Disjoint (PrimState m) a -> Disjoint (PrimState m) a -> m ()
union o1 o2 = stToPrim $ do 
  (d1@(Disjoint v1 vr1), a) <- findValue o1
  (d2@(Disjoint v2 vr2), b) <- findValue o2
  unless (v1 == v2) $ do
    let !c = mappend a b
    r1 <- readMutVar vr1
    r2 <- readMutVar vr2
    case compare r1 r2 of
      LT -> do
        writeMutVar v1 (Left c)
        writeMutVar v2 (Right d1)
      EQ -> do
        writeMutVar vr1 (r1 + 1)
        writeMutVar v1 (Left c)
        writeMutVar v2 (Right d1)
      GT -> do
        writeMutVar v1 (Right d2)
        writeMutVar v2 (Left c)
   -- now notify listeners

singleton :: PrimMonad m => a -> m (Disjoint (PrimState m) a)
singleton a = stToPrim $ Disjoint <$> newMutVar (Left a) <*> newMutVar 0
