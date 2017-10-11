module DominoRecurse where

-- | (row, column, index) triplet, all starting from 0
-- Top left is (0, 0, 0), bottom right for maxPips = 6 is (6, 7, 55)
type Position = (Int, Int, Int)

type Pips = Int

type BonePips = (Pips, Pips)

type BoneNumber = Int

type Bone = (BonePips, BoneNumber)

type Solution = [BoneNumber]

type Puzzle = [Pips]

type Move = (Position, Position, BoneNumber)

dominoRecurse :: IO ()
dominoRecurse = do
  putStrLn "Domino Recurse"
  let solutions = recurse input1 initialPositions initialBones [initialSolution]
  print solutions
  return ()

funDominoRecurse :: Int -> IO ()
funDominoRecurse maxPips = do
  putStrLn "Domino Recurse"
  --  let solutions =
  --        recurse
  --          (inputs !! maxPips)
  --          (funInitialPositions maxPips)
  --          (funInitialBones maxPips)
  --          [funInitialSolution maxPips]
  let solutions =
        recurse2
          (inputs !! maxPips)
          (funInitialPositions maxPips)
          (funInitialBones maxPips)
          (funInitialSolution maxPips)
  print solutions
  return ()

maxPips :: Int
maxPips = 0

input0 :: Puzzle
input0 = [0, 0]

input1 :: Puzzle
input1 = [0, 0, 0, 1, 1, 1]

inputs :: [Puzzle]
inputs = [input0, input1]

initialBones :: [Bone]
initialBones = funInitialBones maxPips

funInitialBones :: Int -> [Bone]
funInitialBones maxPips =
  zip
    [(pips1, pips2) | pips1 <- [0 .. maxPips], pips2 <- [pips1 .. maxPips]]
    [defaultBoneNumber + 1 ..]

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

initialSolution :: Solution
initialSolution = funInitialSolution maxPips

funInitialSolution :: Int -> Solution
funInitialSolution maxPips = replicate (2 * funNumBones maxPips) defaultBoneNumber

defaultBoneNumber :: BoneNumber
defaultBoneNumber = 0

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
     Puzzle -- ^ Numbers of pips to place corresponding bone pips on
  -> [Position] -- ^ Set of available positions
  -> [Bone] -- ^ Set of available bones
  -> [Solution] -- ^ Found solutions, the current being the head
  -> [Solution] -- ^ Solutions
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

recurseTree :: Puzzle -> [Position] -> [Bone] -> Solution -> [Solution]
recurseTree puzzle (position:positions) bones solution = []

bonesWithPips ::
     Pips -- ^ 'Pips' to find on bones
  -> [Bone] -- ^ Available bones
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

pipsOnBone :: Pips -> Bone -> Bool
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

get :: [a] -> Position -> [a]
get xs (_, _, index) = take 1 $ drop index xs

updateList ::
     [a] -- ^ Current list
  -> [Position] -- ^ Positions to update
  -> a -- ^ Value to put at the specified positions
  -> [a] -- ^ Resulting list
updateList xs [] _ = xs
updateList xs ((_, _, index):positions) value =
  updateList (take index xs ++ [value] ++ drop (index + 1) xs) positions value

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold predicateFun headFun tailFun x
  | predicateFun x = []
  | otherwise = headFun x : unfold predicateFun headFun tailFun (tailFun x)

isSolved :: Solution -> Bool
isSolved = notElem defaultBoneNumber

-- | Determine if finished, being any of:
--   - All available positions have been exhausted
--   - All bones left to place have been exhausted
--   - 'Solution' is solved, i.e., isSolved
isFinished :: [Position] -> [Bone] -> Solution -> Bool
isFinished positions bones solution = null positions || null bones || isSolved solution

-- | Determine state validity, i.e. whether or not to be able to continue with a solution
isValid :: [Position] -> [Bone] -> Solution -> Bool
isValid [] [] _      = True
isValid [] _ _       = False
isValid _ [] _       = False
isValid _ _ solution = not $ isSolved solution -- Probably never a case

recurse2 :: Puzzle -> [Position] -> [Bone] -> Solution -> [Solution]
recurse2 puzzle positions bones solution
  | isFinished positions bones solution && valid = [solution]
  | not valid = []
  | otherwise =
    concat
      [ recurse2
        puzzle
        (filterPositions positions moves)
        (filterBones bones moves)
        intermediateSolution
      | intermediateSolution <- [applyMove solution move | move <- moves]
      ]
  where
    valid = isValid positions bones solution
    moves = findMoves puzzle positions bones

findMoves ::
     Puzzle -- ^ 'Puzzle' to solve
  -> [Position] -- ^ Available positions, the head being the one being currently solved
  -> [Bone] -- ^ Available bones
  -> [Move] -- ^ Valid positions and bone numbers
findMoves puzzle (position:positions) bones =
  [ (position, neighbourPosition, boneNumber)
  | ((_, bonePips2), boneNumber) <- bonesWithPips (head $ puzzle `get` position) bones
  , neighbourPosition <- neighboursInSet position positions
  , let neighbourPips = head $ get puzzle neighbourPosition
  , bonePips2 == neighbourPips
  ]

applyMove :: Solution -> Move -> Solution
applyMove solution (position1, position2, boneNumber) =
  updateList solution [position1, position2] boneNumber

filterPositions :: [Position] -> [Move] -> [Position]
filterPositions positions [(position1, position2, _)] =
  filter (\p -> p `notElem` [position1, position2]) positions
filterPositions positions moves =
  [position | move <- moves, position <- filterPositions positions [move]]

filterBones :: [Bone] -> [Move] -> [Bone]
filterBones bones [(_, _, boneNumber)] =
  filter (\(_, boneNumber') -> boneNumber /= boneNumber') bones
filterBones bones moves = [bone | move <- moves, bone <- filterBones bones moves]
