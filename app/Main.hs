module Main (main) where

import Network.Socket
import Network.Transport (Transport)
import Network.Transport.TCP
import System.Environment (getArgs)
import Control.Exception (IOException)
import Control.Distributed.Process.Node

createNode :: HostName -> ServiceName -> IO (Either IOException Transport)
createNode hostname port = createTransport (defaultTCPAddr hostname port) defaultTCPParameters

main :: IO ()
main = do
  args <- getArgs
  case args of
    (port : []) -> do
      transport <- createNode "localhost" port
      case transport of
        Right transport -> do
          putStrLn "Press newline to exit."
          getLine >> putStrLn "Exiting."
        Left ioException -> do
          putStrLn "Could not start the node."
          print ioException
    _          -> do
      putStrLn "Invalid input format. Takes in only a port."
