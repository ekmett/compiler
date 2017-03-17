{-# LANGUAGE DefaultSignatures, TypeFamilies #-}
module Compiler.STM 
  ( MonadSTM(..)
  ) where

import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

class Monad m => MonadSTM m where
  stm :: STM a -> m a
  default stm :: (m ~ t n, MonadTrans t, MonadSTM n) => STM a -> m a
  stm = lift . stm

instance MonadSTM STM where stm = id
instance MonadSTM m => MonadSTM (MaybeT m)
instance MonadSTM m => MonadSTM (ReaderT e m)
instance (MonadSTM m, Monoid w) => MonadSTM (Strict.RWST r w s m)
instance (MonadSTM m, Monoid w) => MonadSTM (Lazy.RWST r w s m)
instance (MonadSTM m, Monoid w) => MonadSTM (Strict.WriterT w m)
instance (MonadSTM m, Monoid w) => MonadSTM (Lazy.WriterT w m)
instance MonadSTM m => MonadSTM (Strict.StateT s m)
instance MonadSTM m => MonadSTM (Lazy.StateT s m)
instance MonadSTM m => MonadSTM (ContT r m)
