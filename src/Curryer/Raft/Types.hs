{-# LANGUAGE DerivingVia #-}

module Curryer.Raft.Types where

import Control.Distributed.Process
import GHC.Generics (Generic)

import Codec.Winery

data CurryTick = CurryTick
  deriving (Generic, Show)
  deriving Serialise via WineryVariant CurryTick
