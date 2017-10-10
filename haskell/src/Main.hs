module Main where

import           DominoTree

main :: IO ()
main = do
  let maxPips = 6
--  dominoEffect maxPips
  dominoTree
  return ()
