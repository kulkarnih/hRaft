module Main (main) where

import Raft.Init

import Config.Parser (parseConfig)
import Config.Types

-- Elixir node communication basics -> https://dev.to/postelxpro/nodes-1jaf

-- Haskell distributed process docs
-- https://hackage.haskell.org/package/distributed-process
-- https://haskell-distributed.github.io/tutorials/1ch.html

main :: IO ()
main = do
  config <- parseConfig
  case config of
    Right (InitConfig port neighbours) -> initRaftApp port neighbours

    Left err -> do
      putStrLn "Invalid config file format."
      print err
