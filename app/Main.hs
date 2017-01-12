module Main where

import           API
import qualified Combo as FC
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  cs <- FC.mkComboState "hn.json"
  run 8080 (app cs)
