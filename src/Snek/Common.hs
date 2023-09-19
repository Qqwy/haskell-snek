module Snek.Common
  ( Direction (..),
    Point,
    gameWidth,
    gameHeight,
    isOppositeDirection,
  )
where

data Direction = North | South | East | West
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Point = (Word, Word)

-- | Width of the playe area, in characters
gameWidth :: Word
gameWidth = 60

-- | Height of the playe area, in characters
gameHeight :: Word
gameHeight = 30

-- | Returns true for North<->South and for East<->West,
--   false otherwise.
isOppositeDirection :: Direction -> Direction -> Bool
isOppositeDirection North South = True
isOppositeDirection South North = True
isOppositeDirection East West = True
isOppositeDirection West East = True
isOppositeDirection _ _ = False
