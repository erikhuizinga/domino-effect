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


