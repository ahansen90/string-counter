module Main
  ( main
  ) where

import Application (start)

main :: IO ()
main = do
  putStrLn "String counter server listening on port 9000"
  start 9000
