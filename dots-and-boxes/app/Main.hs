module Main where

import Lib
import System.Exit(die)
import System.Environment(getArgs, getProgName)

main :: IO ()
main = do args <- getArgs
          case args of 
            [config_file] -> do someFunc
            _ -> do prog <- getProgName
                    die $ "Usage: " ++ prog ++ " <configuration_file>"

