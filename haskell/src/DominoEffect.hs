module DominoEffect where

import           Data.Char
import           System.Console.ANSI (clearScreen)

type BoneNumber = Int

type BonePips = (Int, Int)

type Bone = (BoneNumber, BonePips)

-- | Puzzle contents to be printed on the TUI
data Printable
  = Figure Int
  | Domino Bone
  | Empty

instance Show Printable where
  show (Figure n)                      = show n
  show (Domino (bone, (pips1, pips2))) = show (bone, (pips1, pips2))
  show Empty                           = " . "

type Puzzle = [Printable]

data Orientation
  = SOUTH
  | WEST

-- | Solve the Domino Effect puzzle
dominoEffect ::
     Int -- ^ The maximum number of pips on a bone
  -> IO ()
dominoEffect maxPips = do
  clearScreen
  putWelcome
  putStrLn ("\nUsing up to " ++ show maxPips ++ " pips.")
  putStrLn $ "The initial set of dominoes consists of " ++ show (numBones maxPips) ++ " bones."
  let input = generateSolvableInput maxPips
  putStrLn "\nPuzzle to solve:"
  putPuzzle input
  putStr "\nSolving... "
  let solutions = solve input
  let numSolutions = length solutions
  putStrLn $
    (case numSolutions of
       0 -> "No solutions were"
       1 -> "One solution was"
       _ -> show numSolutions ++ " solutions were") ++
    " found."
  return ()

putWelcome :: IO ()
putWelcome =
  putStr
    ("        D O M I N O\n" ++
     " ___                    ___\n" ++
     "|o o|   E F F E C T    |o o|\n" ++
     "|o_o| ___ ___  ___ ___ |o_o|\n" ++
     "|o  ||o  |ooo||ooo|o o||o o|\n" ++ "|__o||__o|ooo||ooo|o_o||o_o|\n")

-- | Solve a given puzzle by finding all solutions
solve ::
     Puzzle -- ^ Input, i.e. 'Puzzle' to solve
  -> [Puzzle] -- ^ Output, i.e. list of solutions
solve puzzle = [[]]

-- | Draw the puzzle to STDOUT
putPuzzle :: Puzzle -> IO ()
putPuzzle [] = return ()
putPuzzle [printable] = print printable
putPuzzle (printable:printables) = do
  putStr $ show printable
  putPuzzle printables
  return ()

-- | The number of bones to be placed in the puzzle
numBones ::
     Int -- ^ The maximum number of pips on a bone
  -> Int
numBones maxPips = sum [1 .. maxPips + 1]

-- | Cartesian dimensions of the puzzle
puzzleDimensions ::
     Int -- ^ The maximum number of pips on a bone
  -> (Int, Int)
puzzleDimensions maxPips = (maxPips + 1, maxPips + 2)

-- | Calculate the maximum linear index
maxLinearIndex ::
     Int -- ^ The maximum number of pips on a bone
  -> Int
maxLinearIndex maxPips = tupleProduct (puzzleDimensions maxPips) - 1

-- | Linear puzzle indices, starting from 0 from top left along the rows to bottom right
linearIndices ::
     Int -- ^ The maximum number of pips on a bone
  -> [Int]
linearIndices maxPips = [0 .. maxLinearIndex maxPips]

-- | Product of 'Num' tuple
tupleProduct :: Num a => (a, a) -> a
tupleProduct = uncurry (*)

-- | Sum of a 'Num' tuple
tupleSum :: Num a => (a, a) -> a
tupleSum = uncurry (+)

-- | Generate the list of bones at the start of the puzzle
generateBones ::
     Int -- ^ The maximum number of pips on a bone
  -> [Bone]
generateBones maxPips = zip [1 ..] $ generateBonePips maxPips

-- | Generate pips on initial set of bones
generateBonePips ::
     Int -- ^ The maximum number of pips on a bone
  -> [BonePips]
generateBonePips maxPips = [(pips1, pips2) | pips1 <- [0 .. maxPips], pips2 <- [pips1 .. maxPips]]

-- | Generate a solvable input for the puzzle
generateSolvableInput ::
     Int -- ^ The maximum number of pips on a bone
  -> Puzzle
generateSolvableInput maxPips = [Empty]

-- | Determine if puzzle field is full, i.e., if all bones have been placed
isFull :: Puzzle -> Bool
isFull []                = True
isFull (Domino _:fields) = isFull fields
isFull _                 = False
