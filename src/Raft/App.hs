{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Raft.App (
    RaftApp
  , runRaftApp
) where

import Control.Monad.Reader (ReaderT (runReaderT), MonadReader, MonadIO)

import Raft.Types

newtype RaftApp a = RaftApp { unRaftApp :: ReaderT RaftConfig IO a }
  deriving (Functor, Applicative, Monad, MonadReader RaftConfig, MonadIO)

runRaftApp :: RaftApp a -> RaftConfig -> IO a
runRaftApp = runReaderT . unRaftApp
