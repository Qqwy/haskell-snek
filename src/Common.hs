module Common
  where

data Direction = North | South | East | West
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Point = Point { x :: Word, y :: Word }
  deriving (Eq, Ord, Show, Read)

gameWidth :: Word
gameWidth = 30

gameHeight :: Word
gameHeight = 30
