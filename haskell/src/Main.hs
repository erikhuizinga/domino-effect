module Main where

import           DominoRecurse

main :: IO ()
main = do
  let maxPips = 1
--  dominoEffect maxPips
--  dominoTree
  funDominoRecurse maxPips
  return ()
