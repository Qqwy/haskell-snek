{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Lib
     where
import GHC.Generics
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Direction = North | South | East | West
  deriving (Eq, Ord, Show, Read, Generic, Enum, Bounded)

data Snake = Snake { body :: NonEmpty Point, len :: Int }
  deriving (Eq, Ord, Show, Read, Generic)

data Point = Point { x :: Word, y :: Word }
  deriving (Eq, Ord, Show, Read, Generic)

gameWidth :: Word
gameWidth = 30

gameHeight :: Word
gameHeight = 30

data GameState = GameState
  { snake :: Snake
  , direction :: Direction
  , food :: Point
  , score :: Int
  , gameOver :: Bool
  }
  deriving (Eq, Ord, Show, Read, Generic)

initialState :: GameState
initialState = GameState
  { snake = initialSnake
  , direction = North
  , food = Point 0 0
  , score = 0
  , gameOver = False
  }

initialSnake :: Snake
initialSnake = Snake { body = [Point (cx - 1) cy, Point cx cy, Point (cx + 1) cy]
                     , len = 5
                     }
  where
    cx = gameWidth `div` 2
    cy = gameHeight `div` 2

moveSnake :: Direction -> Snake -> Snake
moveSnake dir snake =
  snake { body = new_head :| new_body }
  where
    new_body = NonEmpty.take (snake.len - 1) snake.body
    (Point head_x head_y) = NonEmpty.head snake.body
    new_head =
      case dir of
        North -> Point head_x (head_y - 1)
        South -> Point head_x (head_y + 1)
        East -> Point (head_x + 1) head_y
        West -> Point (head_x - 1) head_y

updateState :: GameState -> GameState
updateState state =
  if gameOver state
  then state
  else
    let
      snake' = moveSnake state.direction state.snake
    in
          state
          { snake = snake'
          , direction = state.direction -- TODO
          , gameOver = snakeOverlaps snake'
          , score = state.score + 1
          , food = state.food -- TODO
          }

snakeOverlaps :: Snake -> Bool
snakeOverlaps (Snake { body = (snake_head :| body) }) = snake_head `elem` body

drawState :: GameState -> String
drawState state = unlines $ [top] <> drawStateRows state <> [bottom]
  where
    top = [0..gameWidth + 2] >> "░"
    bottom = [0..gameWidth + 2] >> "░"

drawStateRows :: GameState -> [String]
drawStateRows state = do
  y <- [0..gameHeight]
  return ("░" <> (drawStateRow y state) <> "░")

drawStateRow :: Word -> GameState -> String
drawStateRow y state = do
  x <- [0..gameWidth]
  drawStatePoint (Point x y) state

drawStatePoint :: Point -> GameState -> String
drawStatePoint point state =
    if state.food == point then
      "O"
    else
      if point `elem` state.snake.body then
        "█"
      else
        " "
