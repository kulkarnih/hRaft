{-# LANGUAGE DeriveGeneric #-}

module Raft.Types(
    RaftMessage
  , RaftState
  , Tick(..)
) where

import Control.Distributed.Process
import GHC.Generics (Generic)
import Data.Data (Typeable)
import Data.Binary (Binary)

-- Use Generic for deriving Serialisable. Typeable to make it type safe.

data RaftMessage =
    Heartbeat { _heartBeatSender :: ProcessId, recipient :: ProcessId }
  | VoteRequest { _voteRequestSender :: ProcessId, recipient :: ProcessId }
  | VoteResponse { recipient :: ProcessId }
  deriving (Show, Generic, Typeable)

data RaftState = Leader | Follower | Candidate
               deriving (Show, Generic, Typeable, Eq)

data Tick = Tick deriving (Show, Generic, Typeable)

instance Binary Tick
