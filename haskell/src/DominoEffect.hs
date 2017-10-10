module DominoEffect where

import           Data.Char
import           System.Console.ANSI (clearScreen)
import           System.Random

type BoneNumber = Int

type BonePips = (Int, Int)

type Bone = (BoneNumber, BonePips)

type CartesianIndex = (Int, Int)

type LinearIndex = Int

-- | Puzzle contents to be printed on the TUI
data Printable
  = Figure Int
  | Domino Bone
  | Empty
  deriving (Eq)

instance Show Printable where
  show (Figure n)                      = show n
  show (Domino (bone, (pips1, pips2))) = show (bone, (pips1, pips2))
  show Empty                           = " . "

type Puzzle = [Printable]

data Direction
  = RIGHT
  | DOWN
  deriving (Show)

-- | Solve the Domino Effect puzzle
dominoEffect ::
     Int -- ^ The maximum number of pips on a bone
  -> IO ()
dominoEffect maxPips = do
  clearScreen
  putWelcome
  putStrLn ("\nUsing up to " ++ show maxPips ++ " pips.")
  putStrLn $ "The initial set of dominoes consists of " ++ show (numBones maxPips) ++ " bones."
  let input = generateInput maxPips
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
  -> LinearIndex
maxLinearIndex maxPips = tupleProduct (puzzleDimensions maxPips) - 1

-- | Linear puzzle indices, starting from 0 from top left along the rows to bottom right
linearIndices ::
     Int -- ^ The maximum number of pips on a bone
  -> [LinearIndex]
linearIndices maxPips = [0 .. maxLinearIndex maxPips]

--linearToCartesian :: LinearIndex -> CartesianIndex
--linearToCartesian i =
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
generateInput ::
     Int -- ^ The maximum number of pips on a bone
  -> Puzzle
generateInput maxPips = generateInput' n $ replicate n Empty
  where
    n = numBones maxPips

generateInput' :: Int -> Puzzle -> Puzzle
generateInput' 0 puzzle = puzzle
generateInput' n puzzle = []

generatePipsPlacedAt ::
     Puzzle -- ^ The 'Puzzle' to generate pips on
  -> LinearIndex -- ^ 'LinearIndex' of an 'Empty' element in the specified 'Puzzle'
  -> Puzzle -- ^ The 'Puzzle' with pips placed on the specified position
generatePipsPlacedAt puzzle i = []

-- | Find first index of an element known to exist
findFirstIndex :: Eq a => a -> [a] -> LinearIndex
findFirstIndex _ [y] = 0 -- | Since we know the element exists, the last recursion must be a hit
findFirstIndex x (y:ys)
  | x == y = 0
  | otherwise = 1 + findFirstIndex x ys

-- | Determine if puzzle field is full, i.e., if all bones have been placed
isFull :: Puzzle -> Bool
isFull []                = True
isFull (Domino _:fields) = isFull fields
isFull _                 = False

-- | Get a random 'IO a' from a range
randomFromRange :: Random a => (a, a) -> IO a
randomFromRange range = getStdRandom (randomR range)

-- | Get a random 'Direction'
randomDirection :: IO Direction
randomDirection = do
  r <- randomRIO (0, 1) :: IO Integer
  return
    (case r of
       0 -> RIGHT
       _ -> DOWN)
