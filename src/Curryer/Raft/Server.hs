{-# LANGUAGE DerivingVia, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Curryer.Raft.Server where

import Network.RPC.Curryer.Client
import Network.RPC.Curryer.Server
import Control.Concurrent.STM
import StmContainers.Map as M
import GHC.Generics
import Codec.Winery
import Data.Functor

import Curryer.Raft.Types
import Network.Socket (ServiceName)
import Raft.Types (Neighbours)
import Data.String (fromString)
import Control.Monad (forM)

-- create data structures to represent remote function calls
data SetKey = SetKey String String
  deriving (Generic, Show)
  deriving Serialise via WineryVariant SetKey

data GetKey = GetKey String
  deriving (Generic, Show)
  deriving Serialise via WineryVariant GetKey

--call the `serve` function to handle incoming requests
initRaftServer :: ServiceName -> Neighbours -> IO ()
initRaftServer port neighbours = do
  -- _ <- fmap (connect [] localHostAddr . fromString ) neighbours
  conns <- forM neighbours (connect [] localHostAddr . fromString ) 
  void $ serve raftRequestHandlers conns localHostAddr (fromString port) Nothing

raftRequestHandlers :: RequestHandlers [Connection]
raftRequestHandlers = [ RequestHandler $ \conns CurryTick -> fmap (call conn) conns]

-- setup incoming request handlers to operate on the server's state
kvRequestHandlers :: RequestHandlers (M.Map String String)
kvRequestHandlers = [ RequestHandler $ \state (SetKey k v) ->
                        atomically $ M.insert v k (connectionServerState state)
                    , RequestHandler $ \state (GetKey k) ->
                        atomically $ M.lookup k (connectionServerState state)
                    ]
