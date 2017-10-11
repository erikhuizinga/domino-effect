module DominoRecurse where

-- | (row, column) pair, starting from 1
type Position = (Int, Int)

type Pips = (Int, Int)

type Bone = (Pips, Int)

dominoRecurse :: IO ()
dominoRecurse = do
  putStrLn "Domino Recurse"
  let solutions = recurse input0 initialPositions maxColumn initialBones []
  print solutions
  return ()

maxPips :: Int
maxPips = 0

input0 :: [Int]
input0 = [0, 0]

input1 :: [Int]
input1 = [0, 0, 0, 1, 1, 1]

initialBones :: [Bone]
initialBones = funInitialBones maxPips

funInitialBones :: Int -> [Bone]
funInitialBones maxPips =
  zip [(pips1, pips2) | pips1 <- [0 .. maxPips], pips2 <- [pips1 .. maxPips]] [1 ..]

initialPositions :: [Position]
initialPositions = funInitialPositions maxPips

funInitialPositions :: Int -> [Position]
funInitialPositions maxPips =
  [(r, c) | r <- [1 .. funMaxRow maxPips], c <- [1 .. funMaxColumn maxPips]]

--initialSolution :: [Int]
--initialSolution = funInitialSolution maxPips
--
--funInitialSolution :: Int -> [Int]
--funInitialSolution maxPips = replicate (2 * funNumBones maxPips) 0
--
maxRow :: Int
maxRow = funMaxRow maxPips

funMaxRow :: Int -> Int
funMaxRow maxPips = maxPips + 1

maxColumn :: Int
maxColumn = funMaxColumn maxPips

funMaxColumn :: Int -> Int
funMaxColumn maxPips = funMaxRow maxPips + 1

numBones :: Int
numBones = funNumBones maxPips

funNumBones :: Int -> Int
funNumBones maxPips = sum [1 .. maxPips + 1]

recurse ::
     [Int] -- ^ Number of pips to place corresponding bone pips on
  -> [Position] -- ^ Set of available positions
  -> Int -- ^ Row length
  -> [Bone] -- ^ Set of available bones
  -> [[Int]] -- ^ Current solutions
  -> [[Int]] -- ^ Solutions
recurse _ [] _ [] solutions = solutions -- All bones have been positioned
recurse _ [] _ _ _ = [] -- No more available positions, discard solution
recurse _ _ _ [] _ = [] -- No more available bones, discard solution
recurse pips (pos:poss) rowLength bones sols =
  [ solution
  | ((_, bonePips), boneNumber) <- bonesWithPips (head pips) bones
  , neighbourPosition <- neighboursInSet pos poss
  , nPips <- get pips [neighbourPosition] rowLength
  , bonePips == nPips
  , let solution = []
  ]

--
--  let pipBones = bonesWithPips pips bones
--      neighbours = neighboursInSet pos poss
--  in []
--
bonesWithPips ::
     Int -- ^ 'Pips' to find on bones
  -> [Bone] -- ^ Available 'Bone' instances
  -> [Bone] -- ^ 'Bone' matches, with the matched pips first in the pips tuple
bonesWithPips pips bones =
  [ ((pips3, pips4), num)
  | ((pips1, pips2), num) <- bones
  , pipsOnBone pips ((pips1, pips2), num)
  -- Ensure the matched pips are always first in the tuple
  , let pips3 = pips
  , let pips4 =
          if pips == pips1
            then pips2
            else pips1
  ]

pipsOnBone :: Int -> Bone -> Bool
pipsOnBone pips ((pips1, pips2), _) = pips `elem` [pips1, pips2]

{-| Get the east (right) and south (down) neighbours of the specified position from the specified
    set of positions
-}
neighboursInSet :: Position -> [Position] -> [Position]
neighboursInSet (r, c) poss = [pos | pos <- [(r, c + 1), (r + 1, c)], pos `elem` poss]

position2Index :: Int -> Position -> Int
position2Index rowLength (row, column) = column - 1 + (row - 1) * rowLength

get :: [a] -> [Position] -> Int -> [a]
get xs positions rowLength =
  [ xs !! index
  | position <- positions
  , let index = position2Index rowLength position
  , index < length xs
  ]

neighbourPips ::
     Position -- ^ 'Position' to get neighbouring pips from
  -> [Position] -- ^ Available positions
  -> Int -- ^ Row lengths
  -> [Int] -- ^ Pips in grid
  -> [Int]
neighbourPips position positions rowLength pips =
  get pips (neighboursInSet position positions) rowLength
