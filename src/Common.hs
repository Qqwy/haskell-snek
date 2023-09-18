module Common
  where

data Direction = North | South | East | West
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Point = (Word, Word)

gameWidth :: Word
gameWidth = 30

gameHeight :: Word
gameHeight = 30
