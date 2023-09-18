{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Snake
  ( Snake(body, len) -- <- Do not export constructor
  , initial
  , move
  , overlaps
  )
  where
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Common(Point(..), Direction(..))
import qualified Common

data Snake = Snake { body :: NonEmpty Point, len :: Int }
  deriving (Eq, Ord, Show, Read)


initial :: Snake
initial = Snake { body = [Point (cx - 1) cy, Point cx cy, Point (cx + 1) cy]
                     , len = 5
                     }
  where
    cx = Common.gameWidth `div` 2
    cy = Common.gameHeight `div` 2


move :: Direction -> Snake -> Snake
move dir snake =
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

overlaps :: Snake -> Bool
overlaps (Snake { body = (snake_head :| body) }) = snake_head `elem` body
