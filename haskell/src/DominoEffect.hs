module DominoEffect where

import           Data.Char

type BoneNumber = Int

type BonePips = (Int, Int)

type Bone = (BoneNumber, BonePips)

-- Puzzle contents to be printed on the TUI
data Printable
  = Figure Int
  | Domino Bone
  | Empty

instance Show Printable where
  show (Figure n)                      = show n
  show (Domino (bone, (pips1, pips2))) = show (bone, (pips1, pips2))
  show Empty                           = " . "

type Puzzle = [Printable]

-- Argument: maxPips, the maximum number of pips on a bone
dominoEffect :: Int -> IO ()
dominoEffect maxPips = do
  putStr "Domino Effect using "
  putChar $ intToDigit maxPips
  putStrLn " as the maximum number of pips."

-- The number of bones to be placed in the puzzle
numBones :: Int -> Int
numBones maxPips = sum [1 .. maxPips + 1]

-- Cartesian dimensions of the puzzle
puzzleDimensions :: Int -> (Int, Int)
puzzleDimensions maxPips = (maxPips + 1, maxPips + 2)

-- Linear puzzle indices, starting from 0 from top left along the rows to bottom right
linearIndices :: Int -> [Int]
linearIndices maxPips = [0 .. tupleProduct (puzzleDimensions maxPips) - 1]

-- Product of Num tuple
tupleProduct :: Num a => (a, a) -> a
tupleProduct = uncurry (*)

-- Sum of a Num tuple
tupleSum :: Num a => (a, a) -> a
tupleSum = uncurry (+)

-- Generate the list of bones at the start of the puzzle
generateBones :: Int -> [Bone]
generateBones maxPips = zip [1 ..] $ generateBonePips maxPips

-- Generate pips on bones
generateBonePips :: Int -> [BonePips]
generateBonePips maxPips = [(pips1, pips2) | pips1 <- [0 .. maxPips], pips2 <- [pips1 .. maxPips]]

-- Generate a solvable input for the puzzle
generateSolvableInput :: Puzzle
generateSolvableInput = [Empty]
