module DominoRecurse where

-- | (row, column, index) triplet, all starting from 0
-- Top left is (0, 0, 0), bottom right for maxPips = 6 is (6, 7, 55)
type Position = (Int, Int, Int)

type Pips = (Int, Int)

type Bone = (Pips, Int)

dominoRecurse :: IO ()
dominoRecurse = do
  putStrLn "Domino Recurse"
  let solutions = recurse input0 initialPositions initialBones [initialSolution]
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
  let maxColumn = funMaxColumn maxPips
  in [ (row, column, index)
     | row <- [0 .. funMaxRow maxPips]
     , column <- [0 .. maxColumn]
     , let index = column + row + row * maxColumn
     ]

initialSolution :: [Int]
initialSolution = funInitialSolution maxPips

funInitialSolution :: Int -> [Int]
funInitialSolution maxPips = replicate (2 * funNumBones maxPips) 0

maxRow :: Int
maxRow = funMaxRow maxPips

funMaxRow :: Int -> Int
funMaxRow maxPips = maxPips

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
  -> [Bone] -- ^ Set of available bones
  -> [[Int]] -- ^ Found solutions, the current being the head
  -> [[Int]] -- ^ Solutions
recurse _ [] [] solutions = solutions -- All bones have been positioned
recurse _ [] _ _ = [] -- No more available positions, discard solution
recurse _ _ [] _ = [] -- No more available bones, discard solution
recurse pips (position:positions) bones (solution:solutions) =
  [ updateList solution [position, neighbourPosition] boneNumber
  -- For all combinations of positions, neighbouring positions and corresponding bone numbers
  | (position, neighbourPosition, boneNumber) <-
      [ (position, neighbourPosition, boneNumber)
      -- For all bones containing 'head pips'
      | ((_, bonePips), boneNumber) <- bonesWithPips (head pips) bones
      -- For all neighbours of that position
      , neighbourPosition <- neighboursInSet position positions
      -- For all pips on the neighbouring positions
      , neighbourPips <- get pips neighbourPosition
      -- Match the other pips on the bone as well
      , bonePips == neighbourPips
      ]
  ] ++
  solutions

--
--  , let solution = updateList sol pos rowLength boneNumber
--  , let solution = updateList solution neighbourPosition rowLength boneNumber
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

-- | Get the east (right) and south (down) neighbours of the specified position from the specified
--   set of positions
neighboursInSet :: Position -> [Position] -> [Position]
neighboursInSet (row, column, _) positions =
  [ (row', column', index)
  | (row', column') <- [(row, column + 1), (row + 1, column)]
  , (row'', column'', index) <- positions -- Get index from available positions
  , row' == row''
  , column' == column''
  ]

--position2Index :: Int -> Position -> Int
--position2Index rowLength (row, column) = column - 1 + (row - 1) * rowLength
--
get :: [a] -> Position -> [a]
get xs (_, _, index) = [xs !! index | index < length xs]

updateList ::
     [Int] -- ^ Current list
  -> [Position] -- ^ Positions to update
  -> Int -- ^ Value to put
  -> [Int] -- ^ Resulting list
updateList xs [] _ = xs
updateList xs ((_, _, index):positions) value =
  updateList (take index xs ++ [value] ++ drop (index + 1) xs) positions value
