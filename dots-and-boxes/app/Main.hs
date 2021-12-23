module Main where

import Lib
import System.Environment (getArgs, getProgName)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [config_file, mode] -> do gameFunc config_file mode
    _ -> do
      prog <- getProgName
      putStrLn $ "Usage: " ++ prog ++ " <configuration_file> <program-mode>"
      putStrLn "'algo-seq': running sequential minimax algorithm ONLY"
      putStrLn "'algo-par': running parallel minimax algorithm ONLY"
      putStrLn "'game-seq': game-mode using sequential minimax algorithm"
      putStrLn "'game-par': game-mode using parallel minimax algorithm"
      die ""
