module Raft.Utils (
    aMicroSecond
  , getLocalNode
  , getNodeId
  , getNodeIds
  , getTransport
  , raftServerName
  , sendMessageToNeighbours
) where

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Exception (IOException)
import Control.Monad (forM_)

import Network.Socket (ServiceName)
import qualified Network.Transport as NT
import Network.Transport.TCP

import Data.Binary (Binary)
import qualified Data.ByteString.Char8 as BSChar
import Data.Data (Typeable)
import Data.String (fromString)

import Raft.Types

raftServerName :: String
raftServerName = "RaftServer"

localHostName :: String
localHostName = "localhost"

getLocalNode :: NT.Transport -> IO LocalNode
getLocalNode = flip newLocalNode initRemoteTable

getTransport :: ServiceName -> IO (Either IOException NT.Transport)
getTransport port = createTransport (defaultTCPAddr localHostName port) defaultTCPParameters

aMicroSecond :: Int
aMicroSecond = 1000000

-- Generate NodeId
-- https://github.com/tweag/ch-nixops-example/blob/master/Main.hs#L77
getNodeId :: String -> String -> NodeId
getNodeId host port = NodeId . NT.EndPointAddress $ BSChar.intercalate ":" [ fromString host, fromString port, "0"]

getNodeIds :: [String] -> [NodeId]
getNodeIds = fmap (getNodeId localHostName)

sendMessageToNeighbours :: (Typeable a, Binary a) => Neighbours -> a -> Process ()
sendMessageToNeighbours nbrs msg = forM_ (getNodeIds nbrs) (\node -> nsendRemote node raftServerName msg)
