{-# LANGUAGE DeriveGeneric #-}

module Raft.Types(
    Neighbours
  , RaftMessage(..)
  , RaftState
  , LocalTick(..)
  , RaftConfig(..)
  , Tick(..)
) where

import Control.Distributed.Process
import GHC.Generics (Generic)
import Data.Data (Typeable)
import Data.Binary (Binary)
import Network.Socket
import Control.Distributed.Process.Internal.Types (LocalNode)

-- Use Generic for deriving Serialisable. Typeable to make it type safe.

type Neighbours = [ServiceName]

data RaftMessage =
    Heartbeat { _heartBeatSender :: ProcessId}
  | VoteRequest { _voteRequestSender :: ProcessId }
  | VoteResponse { recipient :: ProcessId }
  deriving (Show, Generic, Typeable)

data RaftState = Leader | Follower | Candidate
               deriving (Show, Generic, Typeable, Eq)

data ServerState = ServerState {
    _serverRaftState :: RaftState
  , _serverTerm :: Int
}

data RaftConfig = RaftConfig {
    raftConfigLocalNode  :: LocalNode
  , raftConfigPort :: ServiceName
  , raftConfigNeighbours :: Neighbours
}

newtype Tick = Tick ProcessId deriving (Show, Generic, Typeable)

data LocalTick = LocalTick deriving (Show, Generic, Typeable)

instance Binary Tick
instance Binary LocalTick
instance Binary RaftMessage
