module Raft.Init(
    initRaftApp
) where

import Raft.App
import Raft.Client
import Raft.Server
import Raft.Types
import Raft.Utils

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
