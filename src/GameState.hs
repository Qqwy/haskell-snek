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

import qualified Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import System.Random (RandomGen)
import qualified System.Random

import Common (Point(..), Direction(..))
import qualified Common
import Snake (Snake)
import qualified Snake

data GameState = GameState
  { snake :: Snake
  , direction :: Direction
  , foods :: [Point]
  , score :: Int
  , gameOver :: Bool
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
    (x, rng') = System.Random.randomR (0, Common.gameWidth) rng
    (y, rng'') = System.Random.randomR (0, Common.gameHeight) rng'
  in
    Point x y : randomFoods rng''


update :: Maybe Direction -> GameState -> GameState
update new_dir state =
  if gameOver state
  then state
  else
    let
      direction' = Data.Maybe.fromMaybe state.direction new_dir
      snake' = Snake.move direction' state.snake
      foods' = if False then tail state.foods else state.foods
    in
          state
          { snake = snake'
          , direction = direction'
          , gameOver = Snake.overlaps snake'
          , score = state.score + 1
          , foods = foods'
          }

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
  drawPoint (Point x y) state

drawPoint :: Point -> GameState -> String
drawPoint point state
  | point `elem` state.snake.body = "█"
  | point == (head state.foods) = "O"
  | otherwise = " "
