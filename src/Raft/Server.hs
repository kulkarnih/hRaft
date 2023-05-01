module Raft.Server(
    startNode
) where

import Raft.Types

import Network.Socket
import Network.Transport (Transport)
import Network.Transport.TCP

import Control.Concurrent ( threadDelay, forkIO )
import Control.Exception (IOException)
import Control.Monad (forever, void)
import Control.Distributed.Process
import Control.Distributed.Process.Node

import System.Random (randomR, newStdGen)

getTransport :: ServiceName -> IO (Either IOException Transport)
getTransport port = createTransport (defaultTCPAddr "localhost" port) defaultTCPParameters

handleTick :: Tick -> Process ()
handleTick (Tick sender) = do
  liftIO . putStrLn $ "Received a tick from " <> show sender <> "!"

aMicroSecond :: Int
aMicroSecond = 1000000

tickSender :: ProcessId -> Process ()
tickSender sendTo = do
  randomGen <- liftIO newStdGen
  let random = fst $ randomR (aMicroSecond, 2 * aMicroSecond) randomGen :: Int
  liftIO $ threadDelay random
  self <- getSelfPid
  -- Use nsendRemote for sending it to a remote process.
  -- https://hackage.haskell.org/package/distributed-process-0.7.3/docs/Control-Distributed-Process-Internal-Primitives.html#v:nsendRemote
  send sendTo $ Tick self

loopWait :: Process ()
loopWait = do
  receiveWait [match handleTick]
  loopWait

spawnTickNode :: Process ()
spawnTickNode = do
  self <- getSelfPid
  node <- getSelfNode
  register "RaftServer" self
  liftIO $ putStrLn $ "This is pid " <> show self
  liftIO $ putStrLn $ "This is node " <> show node
  maybeProcessId <- whereis "RaftServer"
  liftIO $ putStrLn $ "This is whereis response " <> show maybeProcessId
  void . spawnLocal . forever $ tickSender self
  -- TODO: Looping looks ugly, re-factor.
  loopWait


spawnServer :: Transport -> IO ()
spawnServer transport = do
  tickNode <- newLocalNode transport initRemoteTable
  threadId <- forkIO $ runProcess tickNode spawnTickNode
  putStrLn $ "Started the main listener at " <> show threadId
  putStrLn "Press newline to exit." >> getLine >> putStrLn "Exiting"
  return ()

startNode :: ServiceName -> IO ()
startNode port = do
  transport' <- getTransport port
  either
    (\err -> putStrLn "Could not start the node." >>  print err)
    spawnServer
    transport'
