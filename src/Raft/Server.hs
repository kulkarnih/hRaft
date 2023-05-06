module Raft.Server(
    startNode
) where

import Raft.Types
import Raft.Utils

import Network.Socket
import qualified Network.Transport as NT
import Network.Transport.TCP

import Control.Concurrent ( threadDelay, forkIO )
import Control.Exception (IOException)
import Control.Monad (forever, void, forM_)
import Control.Distributed.Process
import Control.Distributed.Process.Node

import System.Random (randomR, newStdGen)


getTransport :: ServiceName -> IO (Either IOException NT.Transport)
getTransport port = createTransport (defaultTCPAddr "localhost" port) defaultTCPParameters

handleTick :: Tick -> Process ()
handleTick (Tick sender) = do
  liftIO . putStrLn $ "Received a tick from " <> show sender <> "!"

aMicroSecond :: Int
aMicroSecond = 1000000

tickSender :: [NodeId] -> Process ()
tickSender sendTo = do
  randomGen <- liftIO newStdGen
  let random = fst $ randomR (aMicroSecond, 2 * aMicroSecond) randomGen :: Int
  liftIO $ threadDelay random
  self <- getSelfPid
  forM_ sendTo (\node -> nsendRemote node "RaftServer" (Tick self))

getNodeIds :: [String] -> [NodeId]
getNodeIds = fmap (getNodeId "localhost")

loopWait :: Process ()
loopWait = do
  receiveWait [match handleTick]
  loopWait

-- Tick node that sends a tick randomly between 1-2 microseconds
spawnTickNode :: Neighbours -> Process ()
spawnTickNode neighbours = do
  self <- getSelfPid
  node <- getSelfNode
  register "RaftServer" self
  liftIO $ putStrLn $ "This is pid " <> show self
  liftIO $ putStrLn $ "This is node " <> show node
  maybeProcessId <- whereis "RaftServer"
  liftIO $ putStrLn $ "This is whereis response " <> show maybeProcessId
  void . spawnLocal . forever $ tickSender $ getNodeIds neighbours
  -- TODO: Looping looks ugly, re-factor.
  loopWait

-- Main Raft Server
spawnServer :: Neighbours -> NT.Transport -> IO ()
spawnServer neighbours transport = do
  tickNode <- newLocalNode transport initRemoteTable
  threadId <- forkIO $ runProcess tickNode (spawnTickNode neighbours)
  putStrLn $ "Started the main listener at " <> show threadId
  putStrLn "Press newline to exit." >> getLine >> putStrLn "Exiting"
  return ()

startNode :: ServiceName -> Neighbours -> IO ()
startNode port neighbours = do
  transport' <- getTransport port
  either
    (\err -> putStrLn "Could not start the node." >>  print err)
    (spawnServer neighbours)
    transport'
