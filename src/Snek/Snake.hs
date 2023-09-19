module Snek.Snake
  ( Snake (body, len), -- <- Do not export constructor
    initial,
    move,
    overlaps,
    hitsWall,
    snakeHead,
    maybeGrow,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Snek.Common (Direction (..), Point)
import Snek.Common qualified as Common

-- | The protagonist of our little game
data Snake = Snake
  { -- | The snake's body. The first element is the snake's head; also see [snakeHead]
    body :: !(NonEmpty Point),
    -- | The snake's length. [body] can be shorter than [len] if the snake has just eaten food
    len :: !Int
  }
  deriving (Eq, Ord, Show, Read)

-- | Constructs a new snake in the middle of the play area
initial :: Snake
initial =
  Snake
    { body = [(cx - 1, cy), (cx, cy), (cx + 1, cy)],
      len = 5
    }
  where
    cx = Common.gameWidth `div` 2
    cy = Common.gameHeight `div` 2

-- | Helper function to get the snake's head. Also see [body].
snakeHead :: Snake -> Point
snakeHead snake = NonEmpty.head snake.body

move :: Direction -> Snake -> Snake
move dir snake =
  snake {body = new_head :| new_body}
  where
    new_body = NonEmpty.take (snake.len - 1) snake.body
    (headX, headY) = NonEmpty.head snake.body
    new_head =
      case dir of
        North -> (headX, headY - 1)
        South -> (headX, headY + 1)
        East -> (headX + 1, headY)
        West -> (headX - 1, headY)

-- | True if the snake's head is eating its own body
overlaps :: Snake -> Bool
overlaps (Snake {body = (snake_head :| snake_body)}) = snake_head `elem` snake_body

-- | True if the snake's head hits one of the boundaries of the play area
hitsWall :: Snake -> Bool
hitsWall snake =
  let (headX, headY) = snakeHead snake
   in headX < 0
        || headX >= Common.gameWidth
        || headY < 0
        || headY >= Common.gameHeight

-- | Iff the given condition is true, increases the snake's length.
maybeGrow :: Bool -> Snake -> Snake
maybeGrow cond snake
  | cond = snake {len = snake.len + 3}
  | otherwise = snake
