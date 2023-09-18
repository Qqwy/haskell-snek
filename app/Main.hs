{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import qualified Control.Concurrent
import qualified Data.Text
import qualified Data.Text.IO
import qualified System.IO

import Common (Direction(..))
import GameState (GameState)
import qualified GameState

main :: IO ()
main = do
  System.IO.hSetBuffering System.IO.stdin System.IO.NoBuffering
  input_mvar <- Control.Concurrent.newEmptyMVar
  _ <- Control.Concurrent.forkIO (inputLoop input_mvar)
  loop input_mvar GameState.initial

loop :: Control.Concurrent.MVar Direction -> GameState -> IO ()
loop input_mvar state  = do
  new_dir <- Control.Concurrent.tryTakeMVar input_mvar
  let state' = GameState.update new_dir state

  clearScreen
  Data.Text.IO.putStrLn $ "Score: " <> Data.Text.pack (show $ GameState.score state')
  Data.Text.IO.putStr $ GameState.draw state'

  Control.Concurrent.threadDelay 100000
  loop input_mvar state'

inputLoop :: Control.Concurrent.MVar Direction -> IO ()
inputLoop input_mvar = do
  dir <- readDir
  Control.Concurrent.putMVar input_mvar dir
  inputLoop input_mvar

clearScreen :: IO ()
clearScreen = Data.Text.IO.putStrLn "\ESC[2J"

readDir :: IO Direction
readDir = do
  c <- getChar
  case c of
    'w' -> return North
    'a' -> return West
    's' -> return South
    'd' -> return East
    _ -> readDir
