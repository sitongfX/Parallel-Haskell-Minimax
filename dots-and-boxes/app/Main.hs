module Main where

import Lib
import System.Environment (getArgs, getProgName)
import System.Exit (die)
-- import Control.Parallel(par)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [config_file, mode] -> do someFunc config_file mode
    _ -> do
      prog <- getProgName
      die $ "Usage: " ++ prog ++ " <configuration_file> <program-mode>"

-- main :: IO()
-- main = print (nfib2 0)

-- nfib2 :: Integer -> Integer
-- nfib2 n | n < 2 = 1
-- nfib2 n = par nf (nf + nfib2 (n-2) + 1)
--   where nf = nfib2 (n-1)