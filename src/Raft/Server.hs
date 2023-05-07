module Raft.Server(
    initServer
) where

import Raft.App (RaftApp)
import Raft.Types
import Raft.Utils

import Control.Concurrent ( forkIO )

import Control.Distributed.Process
import Control.Distributed.Process.Node

import Control.Monad (forever, forM_)
import Control.Monad.Reader (asks)

-- When you receieve a tick from remote nodes, log them.
handleTick :: Tick -> Process ()
handleTick (Tick sender) = do
  liftIO . putStrLn $ "Received a tick from " <> show sender <> "!"

-- When you receive the tick from local client after timeout send a tick to other nodes.
handleLocalTick :: LocalTick -> Neighbours -> Process ()
handleLocalTick _ neighbours = do
  liftIO . putStrLn $ "Received a local tick! Sending a tick to all neighbours."
  self <- getSelfPid
  forM_ (getNodeIds neighbours) (\node -> nsendRemote node raftServerName (Tick self))

messageHandler :: Neighbours -> Process ()
messageHandler neighbours = do
  -- liftIO $ putStrLn "Waiting in loop for a tick message, local or remote."
  receiveWait [
      match handleTick
    , match $ flip handleLocalTick neighbours
    ]

raftServerInit :: Neighbours -> Process ()
raftServerInit neighbours = do
  self <- getSelfPid
  node <- getSelfNode
  register raftServerName self
  liftIO $ putStrLn $ "This is pid " <> show self
  liftIO $ putStrLn $ "This is node " <> show node
  maybeProcessId <- whereis raftServerName
  liftIO $ putStrLn $ "This is whereis response " <> show maybeProcessId
  forever $ messageHandler neighbours

-- TODO: Check how RaftApp can include Process monad in it. Prevent having to send neighbours.
spawnServer :: Neighbours -> LocalNode -> IO ()
spawnServer neighbours localNode = do
  threadId <- forkIO $ runProcess localNode (raftServerInit neighbours)
  putStrLn $ "Started Raft Server Process at " <> show threadId

initServer :: RaftApp ()
initServer = do
  localNode <- asks raftConfigLocalNode
  nbrs <- asks raftConfigNeighbours
  liftIO $ spawnServer nbrs localNode
