module DominoRecurse where

type Position = (Int, Int)

type Pips = (Int, Int)

type Bone = (Pips, Int)

dominoRecurse :: IO ()
dominoRecurse = do
  putStrLn "Domino Recurse"
  let solutions = recurse input0 initialPositions initialBones [initialSolution input0]
  print solutions
  return ()

maxPips :: Int
maxPips = 0

input0 :: [Int]
input0 = [0, 0]

input1 :: [Int]
input1 = [0, 0, 0, 1, 1, 1]

initialBones = zip [(pips1, pips2) | pips1 <- [0 .. maxPips], pips2 <- [pips1 .. maxPips]] [1 ..]
initialBones :: [Bone]

initialPositions = [(r, c) | r <- [1 .. maxRow], c <- [1 .. maxColumn]]
initialPositions :: [Position]

initialSolution :: [Int] -> [Int]
initialSolution = map (const 0)

maxRow :: Int
maxRow = maxPips + 1

maxColumn :: Int
maxColumn = maxRow + 1

numBones :: Int
numBones = sum [1 .. maxPips + 1]

recurse ::
     [Int] -- ^ Numbers to place bones on
  -> [Position] -- ^ Set of available positions
  -> [Bone] -- ^ Set of available bones
  -> [[Int]] -- ^ Current solutions
  -> [[Int]] -- ^ Solutions
recurse _ [] [] solutions                 = solutions
recurse (n:ns) (pos:poss) bones solutions = []
