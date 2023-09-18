module Common
  where

data Direction = North | South | East | West
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Point = (Word, Word)

gameWidth :: Word
gameWidth = 30

gameHeight :: Word
gameHeight = 30

isOppositeDirection :: Direction -> Direction -> Bool
isOppositeDirection North South = True
isOppositeDirection South North = True
isOppositeDirection East West = True
isOppositeDirection West East = True
isOppositeDirection _ _ = False
