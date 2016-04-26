module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  Right x <- feedList (head args)
  print $ head $ items x
