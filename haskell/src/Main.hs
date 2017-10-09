module Main where

import           DominoEffect

main :: IO ()
main = do
  let maxPips = 6
  dominoEffect maxPips
  return ()
