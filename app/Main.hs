module Main (main) where

import Raft.Server
import Config.Parser (parseConfig)
import Config.Types
-- import Config.Types (InitConfig(InitConfig))

main :: IO ()
main = do
  config <- parseConfig
  case config of
    -- Right (InitConfig port _neighbours) -> startNode (show port)
    Right (InitConfig port neighbours) -> startNode (show port) (fmap show neighbours)
    Left err -> do
      putStrLn "Invalid config file format."
      print err
