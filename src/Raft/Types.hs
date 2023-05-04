{-# LANGUAGE DeriveGeneric #-}

module Raft.Types(
    Neighbours
  , RaftMessage
  , RaftState
  , Tick(..)
) where

import Control.Distributed.Process
import GHC.Generics (Generic)
import Data.Data (Typeable)
import Data.Binary (Binary)
import Network.Socket

-- Use Generic for deriving Serialisable. Typeable to make it type safe.

data RaftMessage =
    Heartbeat { _heartBeatSender :: ProcessId, recipient :: ProcessId }
  | VoteRequest { _voteRequestSender :: ProcessId, recipient :: ProcessId }
  | VoteResponse { recipient :: ProcessId }
  deriving (Show, Generic, Typeable)

data RaftState = Leader | Follower | Candidate
               deriving (Show, Generic, Typeable, Eq)

data ServerConfig = ServerConfig {
    myId  :: ProcessId
  , peers :: [ProcessId]
}

newtype Tick = Tick ProcessId deriving (Show, Generic, Typeable)

type Neighbours = [ServiceName]

instance Binary Tick
