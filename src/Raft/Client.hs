module Raft.Client (
  initClient
) where

import Raft.App (RaftApp)
import Raft.Types
import Raft.Utils

import Control.Concurrent ( forkIO, threadDelay )
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Monad (forever)
import Control.Monad.Reader (asks)

import System.Random (randomR, newStdGen)

tickSender :: Process ()
tickSender = do
  self <- getSelfPid
  node <- getSelfNode
  liftIO $ putStrLn $ "This is client pid " <> show self
  liftIO $ putStrLn $ "This is client node " <> show node
  maybeProcessId <- whereis raftServerName

  forever $ (
    do
      randomGen <- liftIO newStdGen
      let random = fst $ randomR (aMicroSecond, 2 * aMicroSecond) randomGen :: Int
      liftIO $ threadDelay random
      maybe (return ()) (`send` LocalTick) maybeProcessId
      )

spawnClient :: LocalNode -> IO ()
spawnClient localNode = do
  -- threadId <- forkIO $ forever $ runProcess localNode tickSender
  threadId <- forkIO $ runProcess localNode tickSender
  putStrLn $ "Started Raft Client Process at " <> show threadId

initClient :: RaftApp ()
initClient = do
  localNode <- asks raftConfigLocalNode
  liftIO $ spawnClient localNode
