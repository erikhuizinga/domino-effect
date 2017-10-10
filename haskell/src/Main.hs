module Main where

import           DominoRecurse

main :: IO ()
main = do
  let maxPips = 6
--  dominoEffect maxPips
--  dominoTree
  dominoRecurse
  return ()
