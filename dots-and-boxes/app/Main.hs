module Main where

import Lib
import System.Environment (getArgs, getProgName)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [config_file, mode] -> do someFunc config_file mode
    _ -> do
      prog <- getProgName
      die $ "Usage: " ++ prog ++ " <configuration_file> <program-mode>"
