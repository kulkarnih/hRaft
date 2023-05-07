module Main (main) where

import Raft.App
import Raft.Client
import Raft.Server
import Raft.Types
import Raft.Utils


import Config.Parser (parseConfig)
import Config.Types

-- Elixir node communication basics -> https://dev.to/postelxpro/nodes-1jaf

-- Haskell distributed process docs
-- https://hackage.haskell.org/package/distributed-process
-- https://haskell-distributed.github.io/tutorials/1ch.html

initRaftApp :: Integer -> [Integer] -> IO ()
initRaftApp port neighbours = do
  transport <- getTransport (show port)

  either
    (\err -> putStrLn "Could not createTransport." >> print err)
    (\trsp -> do
      localNode <- getLocalNode trsp
      let raftConfig = RaftConfig localNode (show port) (fmap show neighbours)
      putStrLn $ "Initiating on port " <> show port
      runRaftApp initServer raftConfig
      runRaftApp initClient raftConfig
      -- Set the terminal text to red colour.
      putStrLn "\ESC[31mPress newline to exit. \ESC[m" >> getLine >> putStrLn "Exiting"
    )
    transport

main :: IO ()
main = do
  config <- parseConfig
  case config of
    Right (InitConfig port neighbours) -> initRaftApp port neighbours

    Left err -> do
      putStrLn "Invalid config file format."
      print err
