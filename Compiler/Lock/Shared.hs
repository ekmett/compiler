{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Compiler.Lock.Shared
  ( SharedLock
  , newSharedLock, newAcquiredReading, newAcquiredWriting
  , acquireReading, tryAcquireReading, releaseReading, withReading, tryWithReading, waitReading
  , acquireWriting, tryAcquireWriting, releaseWriting, withWriting, tryWithWriting, waitWriting
  -- * unsafe state access
  , SharedLockState(..)
  , unsafeSharedLockState
  ) where

import Compiler.Lock
import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Data
import GHC.Generics

data SharedLockState = Free | Reading {-# UNPACK #-} !Int | Writing
  deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

-- | A multiple-reader single-writer lock
data SharedLock = SharedLock {-# UNPACK #-} !(MVar SharedLockState) {-# UNPACK #-} !Lock {-# UNPACK #-} !Lock
  deriving (Typeable)

instance Eq SharedLock where
  SharedLock s _ _ == SharedLock s' _ _ = s == s'

newSharedLock :: MonadIO m => m SharedLock
newSharedLock = liftIO $ SharedLock <$> newMVar Free <*> newLock <*> newLock
{-# INLINE newSharedLock #-}

newAcquiredReading :: MonadIO m => m SharedLock
newAcquiredReading = liftIO $ SharedLock <$> newMVar (Reading 1) <*> newAcquiredLock <*> newLock
{-# INLINE newAcquiredReading #-}

newAcquiredWriting :: MonadIO m => m SharedLock
newAcquiredWriting = liftIO $ SharedLock <$> newMVar Writing <*> newLock <*> newAcquiredLock
{-# INLINE newAcquiredWriting #-}

acquireReading :: MonadIO m => SharedLock -> m ()
acquireReading (SharedLock s r w) = liftIO $ mask_ go where
  go = takeMVar s >>= \case
    Free -> do
      acquireLock r
      putMVar s $ Reading 1
    Reading n -> putMVar s $! Reading (n + 1)
    Writing -> do
      putMVar s Writing
      waitLock w
      go
{-# INLINE acquireReading #-}

tryAcquireReading :: MonadIO m => SharedLock -> m Bool
tryAcquireReading (SharedLock s r _) = liftIO $ mask_ $ takeMVar s >>= \case
  Free -> do
    acquireLock r
    putMVar s (Reading 1)
    return True
  Reading n -> do
    putMVar s $! Reading (n+1)
    return True
  Writing -> False <$ putMVar s Writing
{-# INLINE tryAcquireReading #-}
  
releaseReading :: MonadIO m => SharedLock -> m ()
releaseReading (SharedLock s r _) = liftIO $ mask_ $ takeMVar s >>= \case
  Reading 1 -> do
    releaseLock r
    putMVar s Free
  Reading n -> putMVar s $ Reading (n-1)
  old -> do
    putMVar s old
    fail "releasing unacquired read lock"
{-# INLINE releaseReading #-}

withReading :: (MonadIO m, MonadMask m) => SharedLock -> m a -> m a
withReading = bracket_ <$> acquireReading <*> releaseReading
{-# INLINE withReading #-}

tryWithReading :: (MonadIO m, MonadMask m) => SharedLock -> m a -> m (Maybe a)
tryWithReading l a = mask $ \restore -> tryAcquireReading l >>= \case
  False -> return Nothing
  True -> do
    r <- restore a `onException` releaseReading l
    releaseReading l
    return (Just r)
{-# INLINE tryWithReading #-}

waitReading :: MonadIO m => SharedLock -> m ()
waitReading l = liftIO $ mask_ $ acquireReading l >> releaseReading l
{-# INLINE waitReading #-}

acquireWriting :: MonadIO m => SharedLock -> m ()
acquireWriting (SharedLock s r w) = liftIO $ mask_ go where
  go = takeMVar s >>= \case
    Free -> do
      acquireLock w
      putMVar s Writing
    Writing -> do
      putMVar s Writing
      waitLock w
      go
    other -> do
      putMVar s other
      waitLock r
      go
{-# INLINE acquireWriting #-}

tryAcquireWriting :: MonadIO m => SharedLock -> m Bool
tryAcquireWriting (SharedLock s _ w) = liftIO $ mask_ $ takeMVar s >>= \case
  Free -> do
    acquireLock w
    putMVar s Writing
    return True
  state -> False <$ putMVar s state
{-# INLINE tryAcquireWriting #-}

releaseWriting :: MonadIO m => SharedLock -> m ()
releaseWriting (SharedLock s _ w) = liftIO $ mask_ $ takeMVar s >>= \case
  Writing -> do
    releaseLock w
    putMVar s Free
  other -> do
    putMVar s other
    fail "releasing unacquired write lock"
{-# INLINE releaseWriting #-}

withWriting :: (MonadIO m, MonadMask m) => SharedLock -> m a -> m a
withWriting = bracket_ <$> acquireWriting <*> releaseWriting
{-# INLINE withWriting #-}

tryWithWriting :: (MonadIO m, MonadMask m) => SharedLock -> m a -> m (Maybe a)
tryWithWriting l a = mask $ \restore -> tryAcquireWriting l >>= \case
  False -> return Nothing
  True -> do
    r <- restore a `onException` releaseWriting l
    releaseWriting  l
    return (Just r)
{-# INLINE tryWithWriting #-}

waitWriting :: MonadIO m => SharedLock -> m ()
waitWriting l = liftIO $ mask_ $ acquireWriting l >> releaseWriting l
{-# INLINE waitWriting #-}

unsafeSharedLockState :: MonadIO m => SharedLock -> m SharedLockState
unsafeSharedLockState (SharedLock s _ _) = liftIO $ do
  state <- readMVar s
  putMVar s state
  return state
{-# INLINE unsafeSharedLockState #-}
