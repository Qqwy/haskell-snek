module Snek.GameState
  ( GameState (snake, direction, foods, score, gameOver), -- <- Do not export constructor
    initial,
    update,
    draw,
  )
where

import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Snek.Common (Direction (..), Point)
import Snek.Common qualified as Common
import Snek.Snake (Snake)
import Snek.Snake qualified as Snake
import System.Random (RandomGen)
import System.Random qualified

-- | Reified state of the game
data GameState = GameState
  {
    -- | The snake
    snake :: !Snake,
    -- | The snake's current direction of travel
    direction :: !Direction,
    -- | An (infinite!) list of next food locations.
    --   The first element is visible on the screen.
    foods :: ![Point],
    -- | The player's score
    score :: !Int,
    -- | True if the game is over; when set to true the game will stop
    gameOver :: !Bool
  }
  deriving (Eq, Ord, Show, Read)

-- | Constructs a new game state.
initial :: (RandomGen rng) => rng -> GameState
initial rng =
  GameState
    { snake = Snake.initial,
      direction = West,
      foods = randomFoods rng,
      score = 0,
      gameOver = False
    }

-- | Constructs an infinite list of random food locations in the play area.
randomFoods :: (RandomGen rng) => rng -> [Point]
randomFoods rng =
  let (px, rng') = System.Random.randomR (0, Common.gameWidth) rng
      (py, rng'') = System.Random.randomR (0, Common.gameHeight) rng'
   in (px, py) : randomFoods rng''

-- | Simulates one game tick.
update :: Maybe Direction -> GameState -> GameState
update new_dir state =
  if gameOver state
    then state
    else
      let direction' = updateDirection new_dir state.direction
          snake' =
            state.snake
              & Snake.move direction'
              & Snake.maybeGrow (isFoodEaten state)
          foods' = if isFoodEaten state then tail state.foods else state.foods
          score' = if isFoodEaten state then state.score + 100 else state.score + 1
       in GameState
            { snake = snake',
              direction = direction',
              gameOver = Snake.overlaps snake' || Snake.hitsWall snake',
              score = score',
              foods = foods'
            }

-- | If the user pressed a key, the snake's direction is updated.
--   However, the snake can never suddenly go backwards.
updateDirection :: Maybe Direction -> Direction -> Direction
updateDirection new_dir dir =
  case new_dir of
    Just new_dir' | not (Common.isOppositeDirection new_dir' dir) -> new_dir'
    _ -> dir

-- | True iff the snake's head is on top of the food
isFoodEaten :: GameState -> Bool
isFoodEaten state =
   head state.foods == Snake.snakeHead state.snake

-- | Renders the game state as a text-based image.
draw :: GameState -> Text
draw state = Text.unlines $ [wall] <> drawRows state <> [wall]
  where
    wall = Text.replicate (fromIntegral $ Common.gameWidth + 3) "░"

drawRows :: GameState -> [Text]
drawRows state = do
  y <- [0 .. Common.gameHeight]
  return ("░" <> drawRow y state <> "░")

drawRow :: Word -> GameState -> Text
drawRow y state = Text.pack $ do
  x <- [0 .. Common.gameWidth]
  drawPoint (x, y) state

drawPoint :: Point -> GameState -> String
drawPoint point state
  | point `elem` state.snake.body = "█"
  | point == head state.foods = "O"
  | otherwise = " "
