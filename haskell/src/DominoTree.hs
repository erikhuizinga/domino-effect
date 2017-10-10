module DominoTree where

data Tree a
  = Leaf a
  | Node a
         [Tree a]

-- | Indices start at zero
type Index = Int

type Position = (Index, Index)

type Pips = Int

type BonePips = (Pips, Pips)

type BoneNumber = Int

type BonePosition = (Position, Position)

type UnpositionedBoneType = (BonePips, BoneNumber)

type PositionedBoneType = (UnpositionedBoneType, BonePosition)

data Bone
  = PositionedBone PositionedBoneType
  | UnpositionedBone UnpositionedBoneType
  deriving (Show)

type BoneTree = Tree Bone

data PuzzleContent
  = Empty
  | PuzzlePips Pips
  | PlacedBone PositionedBoneType

type Puzzle = [PuzzleContent]

instance Show PuzzleContent where
  show Empty = "  .  "
  show (PuzzlePips pips) = "  " ++ show pips ++ "  "
  show (PlacedBone (((pips1, pips2), boneNumber), (position1, position2))) =
    " " ++ show boneNumber ++ "#" ++ show pips1 ++ "|" ++ show pips2 ++ " "

maxPips :: Pips
maxPips = 1

input1 :: Puzzle
input1 = [PuzzlePips p | p <- [0, 0, 0, 1, 1, 1]]

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

isValidPosition :: Position -> Bool
isValidPosition (row, column) = row >= 0 && column >= 0 && row <= maxRow && column <= maxColumn

bonesWithPips ::
     Pips
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

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)
