{-# LANGUAGE LambdaCase #-}
module Compiler.Lock 
  ( Lock
  , newLock, newAcquiredLock
  , acquireLock, tryAcquireLock
  , releaseLock
  , withLock, tryWithLock
  , waitLock
  , unsafeIsUnlockedLock
  ) where

import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Data.Typeable

newtype Lock = Lock (MVar ()) 
  deriving (Eq,Typeable)

newLock :: MonadIO m => m Lock
newLock = liftIO $ Lock <$> newMVar ()
{-# INLINE newLock #-}

newAcquiredLock :: MonadIO m => m Lock
newAcquiredLock = liftIO $ Lock <$> newEmptyMVar
{-# INLINE newAcquiredLock #-}

acquireLock :: MonadIO m => Lock -> m ()
acquireLock (Lock m) = liftIO $ takeMVar m
{-# INLINE acquireLock #-}

tryAcquireLock :: MonadIO m => Lock -> m Bool
tryAcquireLock (Lock m) = liftIO $ isJust <$> tryTakeMVar m
{-# INLINE tryAcquireLock #-}

releaseLock :: MonadIO m => Lock -> m ()
releaseLock (Lock m) = liftIO $ do
  b <- tryPutMVar m ()
  unless b $ fail "releasing unlocked lock"
{-# INLINE releaseLock #-}

withLock :: (MonadIO m, MonadMask m) => Lock -> m a -> m a
withLock = bracket_ <$> acquireLock <*> releaseLock
{-# INLINE withLock #-}

tryWithLock :: (MonadIO m, MonadMask m) => Lock -> m a -> m (Maybe a)
tryWithLock l a = mask $ \restore -> tryAcquireLock l >>= \ case
  False -> return Nothing
  True -> do
    r <- restore a `onException` releaseLock l
    releaseLock l
    return (Just r)
{-# INLINE tryWithLock #-}

waitLock :: MonadIO m => Lock -> m ()
waitLock (Lock m) = liftIO $ mask_ $ takeMVar m >> putMVar m ()
{-# INLINE waitLock #-}

unsafeIsUnlockedLock :: MonadIO m => Lock -> m Bool
unsafeIsUnlockedLock (Lock m) = liftIO $ isEmptyMVar m
{-# INLINE unsafeIsUnlockedLock #-}
