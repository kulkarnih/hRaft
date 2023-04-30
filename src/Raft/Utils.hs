module Raft.Utils(
    startNode
) where

import Raft.Types

import Network.Socket
import Network.Transport (Transport)

import Network.Transport.TCP
import Control.Exception (IOException)
import Control.Distributed.Process.Node
import Control.Distributed.Process


getTransport :: ServiceName -> IO (Either IOException Transport)
getTransport port = createTransport (defaultTCPAddr "localhost" port) defaultTCPParameters

handleTick :: Tick -> Process ()
handleTick _ = do
  liftIO $ putStrLn "Received a tick!"

spawnTickNode :: Process ()
spawnTickNode = do
  self <- getSelfPid
  node <- getSelfNode
  liftIO $ putStrLn $ "This is pid " <> show self
  liftIO $ putStrLn $ "This is node " <> show node
  send self Tick
  receiveWait [match handleTick]

spawnServer :: Transport -> IO ()
spawnServer transport = do
  tickNode <- newLocalNode transport initRemoteTable
  runProcess tickNode spawnTickNode 
  putStrLn "Press newline to exit." >> getLine >> putStrLn "Exiting"
  return ()

startNode :: ServiceName -> IO ()
startNode port = do
  transport' <- getTransport port
  either
    (\err -> putStrLn "Could not start the node." >>  print err)
    spawnServer
    transport'
