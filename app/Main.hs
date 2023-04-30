module Main (main) where

import System.Environment (getArgs)
import Raft.Utils

main :: IO ()
main = do
  args <- getArgs
  case args of
    [port] -> startNode port
    _          ->
      putStrLn "Invalid input format. Takes in only a port."
