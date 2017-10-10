module DominoTree where

data Tree a
  = Leaf a
  | Node a
         [Tree a] -- ^ Any number of subtrees

-- | Indices start at zero
type Index = Int

type Position = (Index, Index)

type Pips = Int

type BonePips = (Pips, Pips)

type BoneNumber = Int

type BonePosition = (Position, Position)

type UnpositionedBoneType = (BonePips, BoneNumber)

type PositionedBoneType = (UnpositionedBoneType, BonePosition)

type PositionedPips = (Position, Pips, BoneNumber)

type PipTree = Tree PositionedPips

data Bone
  = PositionedBone PositionedBoneType
  | UnpositionedBone UnpositionedBoneType
  deriving (Show)

data PuzzleContent
  = Empty
  | PuzzlePips Pips
  | PlacedBone PositionedBoneType

type Puzzle = [PuzzleContent]

instance Show PuzzleContent where
  show Empty = "   .   "
  show (PuzzlePips pips) = "   " ++ show pips ++ "   "
  show (PlacedBone (((pips1, pips2), boneNumber), (position1, position2))) =
    " " ++ show boneNumber ++ "[" ++ show pips1 ++ "|" ++ show pips2 ++ "]"

maxPips :: Pips
maxPips = 1

input1 :: Puzzle
input1 = [PuzzlePips p | p <- [0, 0, 0, 1, 1, 1]]

empty :: Puzzle
empty = replicate numBones Empty

initialBones :: [Bone]
initialBones =
  [ UnpositionedBone unpositionedBone
  | unpositionedBone <-
      zip
        [(pips1, pips2) | pips1 <- [0 .. maxPips], pips2 <- [pips1 .. maxPips]] -- ^ 'BonePips'
        [1 ..] -- ^ 'BoneNumber'
  ]

maxRow :: Index
maxRow = maxPips

maxColumn :: Index
maxColumn = maxPips + 1

numBones :: Int
numBones = (maxRow + 1) * (maxColumn + 1)

-- | Main function
dominoTree :: IO ()
dominoTree = do
  cls
  putStrLn "Domino Tree"
  putStrLn "Initial puzzle:\n"
  putPuzzle input1
  putStrLn "Solving..."
  putStrLn "Intermediate solution:\n"
  putPuzzle empty
  solutions <- solve input1 initialBones
  return ()

isValidPosition :: Position -> Bool
isValidPosition (row, column) = row >= 0 && column >= 0 && row <= maxRow && column <= maxColumn

bonesWithPips ::
     Pips -- ^ 'Pips' to find
  -> [Bone] -- ^ Available 'Bone' instances
  -> [Bone]
bonesWithPips pips = filter (pipsOnBone pips)

pipsOnBone :: Pips -> Bone -> Bool
pipsOnBone pips bone =
  case bone of
    (PositionedBone (unpositionedBone, _)) -> pipsOnUnpositionedBone pips unpositionedBone
    (UnpositionedBone unpositionedBone) -> pipsOnUnpositionedBone pips unpositionedBone

pipsOnUnpositionedBone :: Pips -> UnpositionedBoneType -> Bool
pipsOnUnpositionedBone pips (bonePips, _) = elemPipsBonePips pips bonePips

elemPipsBonePips :: Pips -> BonePips -> Bool
elemPipsBonePips pips (pips1, pips2) = pips `elem` [pips1, pips2]

-- | Clear screen
cls :: IO ()
cls = do
  putStr "\ESC[2J"
  moveCursor (1, 1)
  return ()

moveCursor :: Position -> IO ()
moveCursor (x, y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

showAt :: Position -> String -> IO ()
showAt position string = do
  moveCursor position
  putStr string
  return ()

putPuzzle :: Puzzle -> IO ()
putPuzzle = putStrLn . unlines . map showRow . chop (maxColumn + 1)

showRow :: Show a => [a] -> String
showRow = foldr ((++) . show) ""

-- | Chop list after every nth element
chop ::
     Int -- ^ n
  -> [a]
  -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

solve :: Puzzle -> [Bone] -> IO [Puzzle]
solve puzzle bones = return [] -- TODO

treeSolve :: Pips -> [Bone] -> PipTree
treeSolve pips bones = Leaf ((1, 1), 1, 1)
