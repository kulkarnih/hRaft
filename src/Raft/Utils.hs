module Raft.Utils (
    getNodeId
) where

import Control.Distributed.Process
import qualified Network.Transport as NT
import qualified Data.ByteString.Char8 as BSChar
import Data.String (fromString)

-- Generate NodeId
-- https://github.com/tweag/ch-nixops-example/blob/master/Main.hs#L77
getNodeId :: String -> String -> NodeId
getNodeId host port = NodeId . NT.EndPointAddress $ BSChar.intercalate ":" [ fromString host, fromString port, "0"]

