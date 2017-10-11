-- | Functions to solve the Domino Effect challenge
module DominoEffect where

import           Puzzles

-- | A @(row, column, index)@ triplet, all starting from 0. Top left is @(0, 0, 0)@, bottom right
--  for @maxPips == 6@ is @(6, 7, 55)@
type Position = (Int, Int, Int)

-- | A 'Bone' has two parts with 'Pips'
type BonePips = (Pips, Pips)

-- | Every 'Bone' is uniquely numbered
type BoneNumber = Int

-- | A 'Bone' is another word for domino piece / stone / unit / thingy
type Bone = (BonePips, BoneNumber)

-- | A 'Solution' is a list of 'BoneNumber' corresponding to the 'Bone' of which the 'Pips' match
--   that particular 'Position' in the 'Puzzle'
type Solution = [BoneNumber]

-- | A 'Move' as determined in the algorithm, consisting of the 'BoneNumber' to be put on two
--   'Position's
type Move = (Position, Position, BoneNumber)

-- | In-module script, for which @maxPips@ should be set
dominoEffect :: IO ()
dominoEffect = funDominoEffect maxPips

-- | Function to solve the Domino Effect challenge
funDominoEffect :: Int -> IO ()
funDominoEffect maxPips = do
  putStrLn "Domino Effect"
  putStrLn ""
  --
  let bones = funInitialBones maxPips
  putStrLn "Bones:"
  print bones
  putStrLn ""
  --
  putStrLn "Puzzle:"
  let puzzle = inputs !! maxPips
  print puzzle
  putStrLn ""
  --
  putStr "Solving... "
  let solutions = solve puzzle (funInitialPositions maxPips) bones (funInitialSolution maxPips)
  putStrLn (show (length solutions) ++ " solutuions found! (^_^ )")
  --
  putStrLn ""
  putStrLn "Solutions:"
  print solutions
  return ()

-- | The maximum number of 'Pips' on a 'Bone'
maxPips :: Int
maxPips = 0

-- | The initial set of 'Bone's to solve the puzzle with
initialBones :: [Bone]
initialBones = funInitialBones maxPips

-- | Function to generate the initial set of 'Bone's, depending on the maximum number of 'Pips'
--   on a 'Bone'
funInitialBones :: Int -> [Bone]
funInitialBones maxPips =
  zip
    [(pips1, pips2) | pips1 <- [0 .. maxPips], pips2 <- [pips1 .. maxPips]]
    [defaultBoneNumber + 1 ..]

-- | The initial set of positions in the puzzle, on which bones have to be placed
initialPositions :: [Position]
initialPositions = funInitialPositions maxPips

-- | Function to generate the initial set of positions, depending on the maximum number of pips
--   on a bone
funInitialPositions :: Int -> [Position]
funInitialPositions maxPips =
  let maxColumn = funMaxColumn maxPips
  in [ (row, column, index)
     | row <- [0 .. funMaxRow maxPips]
     , column <- [0 .. maxColumn]
     , let index = column + row + row * maxColumn
     ]

-- | The initial solution, consisting entirely of non-existent bone numbers
initialSolution :: Solution
initialSolution = funInitialSolution maxPips

-- | Function to generate the initial solution, depending on the maximum number of pips on a bone
funInitialSolution :: Int -> Solution
funInitialSolution maxPips = replicate (2 * funNumBones maxPips) defaultBoneNumber

-- | The default bone number, used by no bone in the initial set
defaultBoneNumber :: BoneNumber
defaultBoneNumber = 0

-- | The maximum row index
maxRow :: Int
maxRow = funMaxRow maxPips

-- | Function to calculate the maximum row index, depending on the maximum number of pips on a bone
funMaxRow :: Int -> Int
funMaxRow maxPips = maxPips

-- | The maximum column index
maxColumn :: Int
maxColumn = funMaxColumn maxPips

-- | Function to calculate the maximum column index, depending on the maximum number of pips on a
--   bone
funMaxColumn :: Int -> Int
funMaxColumn maxPips = funMaxRow maxPips + 1

-- | The number of bones in the initial set
numBones :: Int
numBones = funNumBones maxPips

-- | Function to calculate the number of bones in the initial set, depending on the maximum number
--   of pips on a bone
funNumBones :: Int -> Int
funNumBones maxPips = sum [1 .. maxPips + 1]

-- | The recursive algorithm to solve the Domino Effect challenge
solve :: Puzzle -> [Position] -> [Bone] -> Solution -> [Solution]
solve puzzle positions bones solution
  | isSolved solution = [solution]
  | not $ okToContinue positions bones = []
  | otherwise =
    concat
      [ solve
        puzzle
        (filterPositions positions move)
        (filterBones bones move)
        (applyMove solution move)
      | move <- findMoves puzzle positions bones
      ]

-- | Check if a solution solves a puzzle
isSolved :: Solution -> Bool
isSolved = notElem defaultBoneNumber

-- | Check if continuing is possible with the given positions and bones
okToContinue :: [Position] -> [Bone] -> Bool
okToContinue [] _ = False
okToContinue _ [] = False
okToContinue _ _  = True

-- | Remove positions on which a move has been played
filterPositions :: [Position] -> Move -> [Position]
filterPositions positions (position1, position2, _) =
  filter (\p -> p `notElem` [position1, position2]) positions

-- | Remove bones that have been played
filterBones :: [Bone] -> Move -> [Bone]
filterBones bones (_, _, boneNumber) = filter (\(_, boneNumber') -> boneNumber /= boneNumber') bones

-- | Store a move in the intermediate solution by updating it
applyMove :: Solution -> Move -> Solution
applyMove solution (position1, position2, boneNumber) =
  updateList solution [position1, position2] boneNumber

-- | Update a list at the specified positions with the specified value
updateList ::
     [a] -- ^ Current list
  -> [Position] -- ^ Positions to update
  -> a -- ^ Value to put at the specified positions
  -> [a] -- ^ Resulting list
updateList xs [] _ = xs
updateList xs ((_, _, index):positions) value =
  updateList (take index xs ++ [value] ++ drop (index + 1) xs) positions value

-- | Find the moves that place one bone
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

-- | Find boned containing the specified pips
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

-- | Get the element at the specified position, or empty if out of bounds
get :: [a] -> Position -> [a]
get xs (_, _, index) = take 1 $ drop index xs

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

-- | Check if the specified pips are on a bone
pipsOnBone :: Pips -> Bone -> Bool
pipsOnBone pips ((pips1, pips2), _) = pips `elem` [pips1, pips2]
