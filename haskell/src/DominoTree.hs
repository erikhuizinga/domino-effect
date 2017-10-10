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

type UnpositionedBoneType = (BonePips, BoneNumber)

type PositionedBoneType = (UnpositionedBoneType, BonePosition)

data Bone
  = PositionedBone PositionedBoneType
  | UnpositionedBone UnpositionedBoneType
  deriving (Show)

type BoneTree = Tree Bone

data PuzzleContent
  = PuzzlePips Pips
  | PlacedBone PositionedBoneType

type Puzzle = [PuzzleContent]

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

-- | The maximum row index in the puzzle grid
maxRow :: Index
maxRow = maxPips + 1

-- | The maximum column in the puzzle grid
maxColumn :: Index
maxColumn = maxPips

isValidPosition :: Position -> Bool
isValidPosition (row, column) = row >= 0 && column >= 0 && row <= maxRow && column <= maxColumn

findAvailableBonesWithPips ::
     Pips
  -> [Bone] -- ^ Available 'Bone' instances
  -> [Bone]
findAvailableBonesWithPips _ []              = []
findAvailableBonesWithPips pips (bone:bones) = [] -- TODO

arePipsOnBone :: Pips -> Bone -> Bool
arePipsOnBone pips bone =
  case bone of
    (PositionedBone (unpositionedBone, _)) -> arePipsOnUnpositionedBone pips unpositionedBone
    (UnpositionedBone unpositionedBone) -> arePipsOnUnpositionedBone pips unpositionedBone

arePipsOnUnpositionedBone :: Pips -> UnpositionedBoneType -> Bool
arePipsOnUnpositionedBone pips (bonePips, _) = elemPipsBonePips pips bonePips

elemPipsBonePips :: Pips -> BonePips -> Bool
elemPipsBonePips pips (bonePips1, bonePips2) = pips `elem` [bonePips1, bonePips2]
