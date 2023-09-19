module Main (main) where

import Control.Concurrent (MVar)
import Control.Concurrent qualified
import Data.Text qualified
import Data.Text.IO qualified
import Snek.Common (Direction (..))
import Snek.GameState (GameState (..))
import Snek.GameState qualified as GameState
import System.IO qualified
import System.Random qualified

main :: IO ()
main = do
  (input_mvar, state) <- setup
  loop input_mvar state

setup :: IO (MVar Direction, GameState)
setup = do
  System.IO.hSetBuffering System.IO.stdin System.IO.NoBuffering
  input_mvar <- Control.Concurrent.newEmptyMVar
  _ <- Control.Concurrent.forkIO (inputLoop input_mvar)

  rng <- System.Random.newStdGen
  let initial_state = GameState.initial rng

  return (input_mvar, initial_state)

loop :: MVar Direction -> GameState -> IO ()
loop input_mvar state = do
  state' <- update input_mvar state
  render state

  if state.gameOver
    then renderGameOver state'
    else do
      Control.Concurrent.threadDelay (gameSpeed state')
      loop input_mvar state'

update :: MVar Direction -> GameState -> IO GameState
update input_mvar state = do
  new_dir <- Control.Concurrent.tryTakeMVar input_mvar
  return (GameState.update new_dir state)

render :: GameState -> IO ()
render state = do
  clearScreen
  Data.Text.IO.putStrLn $ "Score: " <> Data.Text.pack (show $ GameState.score state)
  Data.Text.IO.putStr $ GameState.draw state

renderGameOver :: GameState -> IO ()
renderGameOver state =
  Data.Text.IO.putStrLn $ "Game Over. Final score: " <> Data.Text.pack (show $ GameState.score state)

-- Because terminal input is blocking, we run a separate thread that waits for input.
inputLoop :: Control.Concurrent.MVar Direction -> IO ()
inputLoop input_mvar = do
  dir <- readDir
  Control.Concurrent.putMVar input_mvar dir
  inputLoop input_mvar

clearScreen :: IO ()
clearScreen = Data.Text.IO.putStrLn "\ESC[H\ESC[2J"

readDir :: IO Direction
readDir = do
  c <- getChar
  case c of
    'w' -> return North
    'a' -> return West
    's' -> return South
    'd' -> return East
    _ -> readDir

gameSpeed :: GameState -> Int
gameSpeed state = case state.score of
  s | s < 100 -> 200000
  s | s < 200 -> 150000
  s | s < 300 -> 100000
  s | s < 400 -> 80000
  s | s < 500 -> 60000
  s | s < 750 -> 50000
  s | s < 1000 -> 40000
  s | s < 2000 -> 30000
  s | s < 3000 -> 20000
  s | s < 4000 -> 15000
  _ -> 10000
