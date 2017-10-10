module DominoTree where

data Tree a
  = Leaf a
  | Node a
         [Tree a]

type Position = (Int, Int)

type Pips = Int

type BonePips = (Pips, Pips)

type BoneNumber = Int

type BonePosition = (Position, Position)

type Bone = (BonePips, BoneNumber, BonePosition)

type BoneTree = Tree Bone


