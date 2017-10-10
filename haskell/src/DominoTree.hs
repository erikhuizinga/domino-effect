module DominoTree where

data Tree a
  = Leaf a
  | Node a
         [Tree a]

-- | An index starts at zero
type Index = Int

type Position = (Index, Index)

type Pips = Int

type BonePips = (Pips, Pips)

type BoneNumber = Int

type BonePosition = (Position, Position)

type Bone = (BonePips, BoneNumber)

type PositionedBone = (Bone, BonePosition)

type BoneTree = Tree Bone

data PuzzleContent
  = PuzzlePips Pips
  | PlacedBone PositionedBone

type Puzzle = [PuzzleContent]

maxPips :: Pips
maxPips = 1

input1 :: Puzzle
input1 = [PuzzlePips p | p <- [0, 0, 0, 1, 1, 1]]

initialBones :: [Bone]
initialBones =
  zip
    [(pips1, pips2) | pips1 <- [0 .. maxPips], pips2 <- [pips1 .. maxPips]] -- ^ 'BonePips'
    [1 ..] -- ^ 'BoneNumber'

-- | The maximum row index in the puzzle grid
maxRow :: Index
maxRow = maxPips + 1

-- | The maximum column in the puzzle grid
maxColumn :: Index
maxColumn = maxPips

isValidPosition :: Position -> Bool
isValidPosition (row, column) = row >= 0 && column >= 0 && row <= maxRow && column <= maxColumn


