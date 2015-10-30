module Main where

import qualified FoldVersion
import qualified ReactiveVersion
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["fold"] -> FoldVersion.exec
    ["reactive"] -> ReactiveVersion.exec
    _ -> putStrLn "Usage: slow-frp <test-type>"
