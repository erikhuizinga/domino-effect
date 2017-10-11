module DominoRecurse where

import           Puzzles

-- | (row, column, index) triplet, all starting from 0
-- Top left is (0, 0, 0), bottom right for maxPips = 6 is (6, 7, 55)
type Position = (Int, Int, Int)

type BonePips = (Pips, Pips)

type BoneNumber = Int

type Bone = (BonePips, BoneNumber)

type Solution = [BoneNumber]

type Move = (Position, Position, BoneNumber)

dominoRecurse :: IO ()
dominoRecurse = funDominoRecurse maxPips

funDominoRecurse :: Int -> IO ()
funDominoRecurse maxPips = do
  putStrLn "Domino Recurse"
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
  let solutions = recurse puzzle (funInitialPositions maxPips) bones (funInitialSolution maxPips)
  putStrLn (show (length solutions) ++ " solutuions found! (^_^ )")
  --
  putStrLn ""
  putStrLn "Solutions:"
  print solutions
  return ()

maxPips :: Int
maxPips = 0

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

recurse :: Puzzle -> [Position] -> [Bone] -> Solution -> [Solution]
recurse puzzle positions bones solution
  | isSolved solution = [solution]
  | not $ okToContinue positions bones = []
  | otherwise =
    concat
      [ recurse
        puzzle
        (filterPositions positions move)
        (filterBones bones move)
        (applyMove solution move)
      | move <- findMoves puzzle positions bones
      ]

isSolved :: Solution -> Bool
isSolved = notElem defaultBoneNumber

okToContinue :: [Position] -> [Bone] -> Bool
okToContinue [] _ = False
okToContinue _ [] = False
okToContinue _ _  = True

filterPositions :: [Position] -> Move -> [Position]
filterPositions positions (position1, position2, _) =
  filter (\p -> p `notElem` [position1, position2]) positions

filterBones :: [Bone] -> Move -> [Bone]
filterBones bones (_, _, boneNumber) = filter (\(_, boneNumber') -> boneNumber /= boneNumber') bones

applyMove :: Solution -> Move -> Solution
applyMove solution (position1, position2, boneNumber) =
  updateList solution [position1, position2] boneNumber

updateList ::
     [a] -- ^ Current list
  -> [Position] -- ^ Positions to update
  -> a -- ^ Value to put at the specified positions
  -> [a] -- ^ Resulting list
updateList xs [] _ = xs
updateList xs ((_, _, index):positions) value =
  updateList (take index xs ++ [value] ++ drop (index + 1) xs) positions value

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

pipsOnBone :: Pips -> Bone -> Bool
pipsOnBone pips ((pips1, pips2), _) = pips `elem` [pips1, pips2]
