{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Compiler.Prop where

import Compiler.STM
import Compiler.STM.Chan
import Control.Concurrent.STM
import Control.Monad.Trans.Cont
import Data.Hashable
import Data.IntMap.Strict as IntMap
import Data.Profunctor
import Data.Reflection
import Data.Typeable
import Data.Set as Set

data NetworkState = NetworkState {-# UNPACK #-} !Int !(IntMap (Set Int)) 
newtype Network = Network (TVar NetworkState)
  deriving (Eq,Typeable)

withNetwork :: (forall s. Reifies s Network => Proxy s -> STM r) -> STM r
withNetwork f = do
  tv <- newTVar (NetworkState 0 IntMap.empty)
  reify (Network tv) f

data Cell s a b = Cell !Int (a -> STM ()) (STM (RChan b))
  deriving Typeable

instance Profunctor (Cell s) where
  dimap l r (Cell i p g) = Cell i (p . l) (fmap r <$> g)
  rmap r (Cell i p g) = Cell i p (fmap r <$> g)
  lmap l (Cell i p g) = Cell i (p . l) g

instance Eq (Cell s a b) where
  Cell i _ _ == Cell j _ _ = i == j

instance Ord (Cell s a b) where
  Cell i _ _ `compare` Cell j _ _ = compare i j

instance Hashable (Cell s a b) where
  hashWithSalt s (Cell i _ _) = hashWithSalt s i

data Prop s = Prop {-# UNPACK #-} !Int !(STM ()) !(STM Bool)

instance Eq (Prop s) where
  Prop i _ _ == Prop j _ _ = i == j

instance Ord (Prop s) where
  Prop i _ _ `compare` Prop j _ _ = compare i j

instance Hashable (Prop s) where
  hashWithSalt s (Prop i _ _) = hashWithSalt s i

quit :: ContT () STM a
quit = ContT $ \_ -> return () 

cell :: forall m s a b. (MonadSTM m, Reifies s Network) => (a -> ContT () STM b) -> m (Cell s a b)
cell f = stm $ do
  let Network tv = reflect (Proxy :: Proxy s)
  NetworkState i m <- readTVar tv
  writeTVar tv $! NetworkState (i + 1) m
  c <- newBroadcastTChan 
  return $! Cell i (\a -> runContT (f a) (writeTChan c)) (RChan id <$> dupTChan c)

write :: Cell s a b -> a -> STM ()
write (Cell _ cp _) a = cp a

prop :: forall m s a a' b b'. (MonadSTM m, Reifies s Network) => (RChan a' -> ContT () STM b) -> Cell s a a' -> Cell s b b' -> m (Prop s)
prop f (Cell ci _ cg) (Cell di dp _) = stm $ do
  let Network tv = reflect (Proxy :: Proxy s)
  NetworkState ip m <- readTVar tv
  writeTVar tv $! NetworkState (ip + 1) $
    IntMap.insertWith Set.union ci (Set.singleton ip) $
    IntMap.insert ip (Set.singleton di) m
  c' <- cg
  return $! Prop ip (runContT (f c') dp) (not <$> isEmptyChan c')

prop2 :: forall m s a a' b b' c c'. (MonadSTM m, Reifies s Network) => (RChan a' -> RChan b' -> ContT () STM c) -> Cell s a a' -> Cell s b b' -> Cell s c c' -> m (Prop s)
prop2 f (Cell ci _ cg) (Cell di _ dg) (Cell ei ep _) = stm $ do
  let Network tv = reflect (Proxy :: Proxy s)
  NetworkState ip m <- readTVar tv
  writeTVar tv $! NetworkState (ip + 1) $ 
    IntMap.insertWith Set.union ci (Set.singleton ip) $
    IntMap.insertWith Set.union di (Set.singleton ip) $
    IntMap.insert ip (Set.singleton ei) m
  c' <- cg
  d' <- dg
  return $! Prop ip (runContT (f c' d') ep) $ 
    isEmptyChan c' >>= \case 
      False -> return True
      True -> not <$> isEmptyChan d' 
