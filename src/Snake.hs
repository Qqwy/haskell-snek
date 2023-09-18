{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Snake
  ( Snake(body, len) -- <- Do not export constructor
  , initial
  , move
  , overlaps
  , hitsWall
  , snakeHead
  , maybeGrow
  )
  where
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Common(Point, Direction(..))
import qualified Common

data Snake = Snake { body :: !(NonEmpty Point), len :: !Int }
  deriving (Eq, Ord, Show, Read)


initial :: Snake
initial = Snake { body = [(cx - 1, cy), (cx, cy), (cx + 1, cy)]
                     , len = 5
                     }
  where
    cx = Common.gameWidth `div` 2
    cy = Common.gameHeight `div` 2

snakeHead :: Snake -> Point
snakeHead snake = NonEmpty.head snake.body

move :: Direction -> Snake -> Snake
move dir snake =
  snake { body = new_head :| new_body }
  where
    new_body = NonEmpty.take (snake.len - 1) snake.body
    (headX, headY) = NonEmpty.head snake.body
    new_head =
      case dir of
        North -> (,) headX (headY - 1)
        South -> (,) headX (headY + 1)
        East -> (,) (headX + 1) headY
        West -> (,) (headX - 1) headY

overlaps :: Snake -> Bool
overlaps (Snake { body = (snake_head :| body) }) = snake_head `elem` body

hitsWall :: Snake -> Bool
hitsWall snake =
  let
    (headX, headY) = snakeHead snake
  in
    False
    || headX < 0
    || headX >= Common.gameWidth
    || headY < 0
    || headY >= Common.gameHeight

maybeGrow :: Bool -> Snake -> Snake
maybeGrow cond snake
  | cond = snake { len = snake.len + 3}
  | otherwise = snake
