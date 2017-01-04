module Main where

import           API
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8080 app
