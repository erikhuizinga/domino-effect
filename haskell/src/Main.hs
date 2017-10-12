-- | The main module for the Domino Effect puzzle
module Main where

import           DominoEffect

-- | Run the Domino Effect challenge
main :: IO ()
main = do
  let maxPips = 3
  --  funDominoEffect maxPips
  dominoEffect maxPips
  return ()
