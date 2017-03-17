{-# LANGUAGE GADTs #-}
module Compiler.STM.Chan 
  ( WriteChan(..)
  , ReadChan(..)
  , RChan(..), rchan
  ) where

import Compiler.STM
import Control.Concurrent.STM

class WriteChan c where
  writeChan :: MonadSTM m => c a -> a -> m ()

instance WriteChan TChan where
  writeChan c a = stm (writeTChan c a)

class ReadChan c where
  readChan :: MonadSTM m => c a -> m a
  tryReadChan :: MonadSTM m => c a -> m (Maybe a)
  peekChan :: MonadSTM m => c a -> m a
  tryPeekChan :: MonadSTM m => c a -> m (Maybe a)
  isEmptyChan :: MonadSTM m => c a -> m Bool
  cloneChan :: MonadSTM m => c a -> m (c a)
  dupChan :: MonadSTM m => c a -> m (c a)

instance ReadChan TChan where
  readChan = stm . readTChan
  tryReadChan = stm . tryReadTChan
  peekChan = stm . peekTChan
  tryPeekChan = stm . tryPeekTChan
  isEmptyChan = stm . isEmptyTChan
  cloneChan = stm . cloneChan
  dupChan = stm . dupChan

-- Coyoneda TChan
data RChan a where
  RChan :: (x -> a) -> {-# UNPACK #-} !(TChan x) -> RChan a

rchan :: TChan a -> RChan a
rchan = RChan id

instance Functor RChan where
  fmap f (RChan g tc) = RChan (f . g) tc
  a <$ RChan _ tc = RChan (const a) tc

instance ReadChan RChan where
  readChan (RChan f tc) = stm $ f <$> readTChan tc
  tryReadChan (RChan f tc) = stm $ fmap f <$> tryReadTChan tc
  peekChan (RChan f tc) = stm $ f <$> peekTChan tc
  tryPeekChan (RChan f tc) = stm $ fmap f <$> tryPeekTChan tc
  isEmptyChan (RChan _ tc) = stm $ isEmptyTChan tc
  cloneChan (RChan f tc) = stm $ RChan f <$> cloneTChan tc
  dupChan (RChan f tc) = stm $ RChan f <$> dupTChan tc
