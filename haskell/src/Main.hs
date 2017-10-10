module Main where

import           DominoEffect
import           DominoTree

main :: IO ()
main = do
  let maxPips = 6
  dominoEffect maxPips
  return ()
