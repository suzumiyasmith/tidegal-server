{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tide.Server.Types where

import Data.Binary
import GHC.Generics

import Linear
import Control.Lens
import qualified Data.Set as S

import Network.WebSockets

import Control.Concurrent

import Tide.Types

import Physics.Light

import Apecs
import Apecs.TH (makeWorld)
import Apecs.Stores (Cache)

type SS = Chan (UserName, Connection)

instance Component Object2D where
  type Storage Object2D = Map Object2D

data Player1 = Player1
instance Component Player1 where
  type Storage Player1 = Unique Player1

data Player2 = Player2
instance Component Player2 where
  type Storage Player2 = Unique Player2

data Team = Team Int
instance Component Team where
  type Storage Team = Map Team

data Bullet = Bullet
instance Component Bullet where
  type Storage Bullet = Map Bullet

data Controller = Controller (MVar PlayerInput)
instance Component Controller where
  type Storage Controller = Map Controller

data Camera = Camera (Chan DisplayData)
instance Component Camera where
  type Storage Camera = Map Camera

data MoveDirection
  = MovingUp
  | MovingDown
  | MovingLeft
  | MovingRight
  deriving (Eq, Ord, Show)

data Effect
  = Fire Card
  | LifeRuin Double
  deriving (Eq, Ord, Show)

data Card
  = CardIceBall
  | CardFireBall
  deriving (Eq, Ord, Show)

data PlayerState = PlayerState
  { _moveState :: S.Set MoveDirection
  , _fireState :: ([Card],[Card])
  } deriving (Eq, Ord, Show)

instance Component PlayerState where
  type Storage PlayerState = Map PlayerState

makeLenses ''PlayerState

data ShipState = ShipState
  { _effectQueue :: [Effect]
  } deriving (Eq, Ord, Show)

instance Component ShipState where
  type Storage ShipState = Map ShipState

makeLenses ''ShipState

makeWorld "GameWorld" [''Object2D, ''Player1, ''Player2, ''Team, ''Bullet, ''Controller, ''Camera, ''PlayerState, ''ShipState]
