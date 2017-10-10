module DominoRecurse where

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

initialBones :: [((Int, Int), Int)]
initialBones = zip [(pips1, pips2) | pips1 <- [0 .. maxPips], pips2 <- [pips1 .. maxPips]] [1 ..]

initialPositions :: [(Int, Int)]
initialPositions = [(r, c) | r <- [1 .. maxRow], c <- [1 .. maxColumn]]

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
  -> [(Int, Int)] -- ^ Set of available positions
  -> [((Int, Int), Int)] -- ^ Set of available bones
  -> [[Int]] -- ^ Current solutions
  -> [[Int]] -- ^ Solutions
recurse _ [] [] solutions                 = solutions
recurse (n:ns) (pos:poss) bones solutions = []
