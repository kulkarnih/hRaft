module Raft.Server(
    startNode
) where

import Raft.Types

import Network.Socket
import qualified Network.Transport as NT
import Network.Transport.TCP

import Control.Concurrent ( threadDelay, forkIO )
import Control.Exception (IOException)
import Control.Monad (forever, void, forM_)
import Control.Distributed.Process
import Control.Distributed.Process.Node

import qualified Data.ByteString.Char8 as BSChar

import System.Random (randomR, newStdGen)
import Data.String (IsString(fromString))

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

-- Generate NodeId
-- https://github.com/tweag/ch-nixops-example/blob/master/Main.hs#L77
getNodeId :: String -> NodeId
getNodeId port = NodeId . NT.EndPointAddress $ BSChar.intercalate ":" [ "localhost", fromString port, "0"]

getNodeIds :: [String] -> [NodeId]
getNodeIds = fmap getNodeId

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
  void . spawnLocal . forever $ tickSender $ getNodeIds ["8080", "8081"]
  -- TODO: Looping looks ugly, re-factor.
  loopWait


spawnServer :: NT.Transport -> IO ()
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
