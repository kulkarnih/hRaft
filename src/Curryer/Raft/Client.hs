{-# LANGUAGE DerivingVia, DeriveGeneric, OverloadedStrings #-}

module Curryer.Raft.Client where

import GHC.Generics
import Network.RPC.Curryer.Client
import Network.RPC.Curryer.Server (localHostAddr)
import Codec.Winery
import Network.Socket (ServiceName)
import Raft.Types (Neighbours)
import System.Random (randomR, newStdGen)
import Control.Monad (forever)
import Curryer.Raft.Types (CurryTick(CurryTick))
import Data.String (fromString)
import Control.Concurrent (forkIO, threadDelay)

aMicroSecond :: Int
aMicroSecond = 1000000

data SetKey = SetKey String String
  deriving (Generic, Show)
  deriving Serialise via WineryVariant SetKey

data GetKey = GetKey String
  deriving (Generic, Show)
  deriving Serialise via WineryVariant GetKey


-- tickSender :: [NodeId] -> Process ()
tickSender conn = do
  randomGen <- newStdGen
  let random = fst $ randomR (aMicroSecond, 2 * aMicroSecond) randomGen :: Int
  threadDelay random
  _ <- call conn CurryTick
  return ()

startClient :: ServiceName -> IO ()
startClient port = do
  -- Create a connection to the Raft Server
  conn <- connect [] localHostAddr port
  _ <- forkIO $ forever $ tickSender conn
  return ()
