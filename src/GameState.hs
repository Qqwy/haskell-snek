{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module GameState
  ( GameState(snake, direction, foods, score, gameOver) -- <- Do not export constructor
  , initial
  , update
  , draw
  )
  where

import Data.Function ((&))
import qualified Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import System.Random (RandomGen)
import qualified System.Random

import Common (Point, Direction(..))
import qualified Common
import Snake (Snake)
import qualified Snake

data GameState = GameState
  { snake :: !Snake
  , direction :: !Direction
  , foods :: [Point]
  , score :: !Int
  , gameOver :: !Bool
  }
  deriving (Eq, Ord, Show, Read)

initial :: RandomGen rng => rng -> GameState
initial rng = GameState
  { snake = Snake.initial
  , direction = North
  , foods = randomFoods rng
  , score = 0
  , gameOver = False
  }

randomFoods :: RandomGen rng => rng -> [Point]
randomFoods rng =
  let
    (px, rng') = System.Random.randomR (0, Common.gameWidth) rng
    (py, rng'') = System.Random.randomR (0, Common.gameHeight) rng'
  in
    (px, py) : randomFoods rng''


update :: Maybe Direction -> GameState -> GameState
update new_dir state =
  if gameOver state
  then state
  else
    let
      direction' = Data.Maybe.fromMaybe state.direction new_dir
      snake' =
        state.snake
        & Snake.move direction'
        & Snake.maybeGrow (foodEaten state)
      foods' = if foodEaten state then tail state.foods else state.foods
      score' = if foodEaten state then state.score + 100 else state.score + 1
    in
          state
          { snake = snake'
          , direction = direction'
          , gameOver = Snake.overlaps snake' || Snake.hitsWall snake'
          , score = score'
          , foods = foods'
          }


foodEaten :: GameState -> Bool
foodEaten state =
  let food = head state.foods
  in
    food == Snake.snakeHead state.snake

draw :: GameState -> Text
draw state = Text.unlines $ [wall] <> drawRows state <> [wall]
  where
    wall    = Text.replicate (fromIntegral $ Common.gameWidth + 3) "░"

drawRows :: GameState -> [Text]
drawRows state = do
  y <- [0..Common.gameHeight]
  return ("░" <> (drawRow y state) <> "░")

drawRow :: Word -> GameState -> Text
drawRow y state = Text.pack $ do
  x <- [0..Common.gameWidth]
  drawPoint (x, y) state

drawPoint :: Point -> GameState -> String
drawPoint point state
  | point `elem` state.snake.body = "█"
  | point == (head state.foods) = "O"
  | otherwise = " "
