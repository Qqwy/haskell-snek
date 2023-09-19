# Snek

This repo is an example implementation of the game of Snake, running in the terminal.


## How to build/run

Install GHC and Stack by following [the instructions of GHCUp](https://www.haskell.org/ghcup/)

- Run `stack run` to first create an executable and then run it
- Run `stack repl` to load the modules into GHCi where you can interact with them

## Design choices

This project is intended to be simple. As such, instead of using a full-fledged terminal-UI library (like [vty](https://hackage.haskell.org/package/vty) or [brick](https://hackage.haskell.org/package/brick)) it contains the code of what little it needs to do user-interaction at the terminal itself.

This means:
- Terminal drawing is done in a primitive fashion, which means that the game might sometimes 'flicker' a little, and when quitting you will see earlier states of the game when you scroll up.
- As Snake is a real-time game, reading user input has to happen in non-blocking fashion. However, reading from standard input is blocking. To circumvent the problem, we start a second thread which is allowed to block. Whenever a new keypress happens, this is communicated to the main thread using an MVar.

Another design choice (but a common one for Haskell projects), is to follow the 'imperative shell, pure core' idiom. This makes the different parts very easy to test and refactor.
