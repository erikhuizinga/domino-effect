-- | Puzzles for the domino effect challenge
module Puzzles where

-- | Dots or points on a domino stone, a.k.a. bone
type Pips = Int

-- | A 'Puzzle' consists of a list of 'Pips', on which bones are to be placed
type Puzzle = [Pips]

-- | For when @maxPips == 0@
input0 :: Puzzle
input0 = [0, 0]

-- | For when @maxPips == 1@
input1 :: Puzzle
input1 = [0, 0, 0, 1, 1, 1]

-- | For when @maxPips == 2@
input2 :: Puzzle
input2 = [2, 1, 0, 1, 2, 0, 2, 1, 2, 0, 0, 1]

-- | For when @maxPips == 3@
input3 :: Puzzle
input3 = [2, 0, 0, 3, 1, 2, 3, 1, 3, 0, 2, 1, 0, 3, 2, 0, 2, 3, 1, 1]

-- | For when @maxPips == 4@
input4 :: Puzzle
input4 = [3, 3, 1, 3, 2, 2, 3, 2, 0, 4, 2, 2, 4, 4, 1, 1, 1, 3, 4, 4, 1, 0, 0, 0, 4, 3, 2, 0, 0, 1]

-- | For when @maxPips == 5@
input5 :: Puzzle
input5 =
  [ 3
  , 3
  , 1
  , 3
  , 2
  , 2
  , 2
  , 3
  , 2
  , 0
  , 4
  , 2
  , 2
  , 5
  , 4
  , 4
  , 1
  , 1
  , 1
  , 3
  , 0
  , 4
  , 4
  , 1
  , 0
  , 0
  , 0
  , 5
  , 4
  , 3
  , 2
  , 0
  , 0
  , 1
  , 4
  , 3
  , 5
  , 5
  , 5
  , 5
  , 1
  , 5
  ]

-- | For when @maxPips == 6@
input6 :: Puzzle
input6 =
  [ 3
  , 3
  , 1
  , 3
  , 2
  , 2
  , 2
  , 2
  , 3
  , 2
  , 0
  , 4
  , 2
  , 2
  , 5
  , 6
  , 4
  , 4
  , 1
  , 1
  , 1
  , 3
  , 0
  , 6
  , 4
  , 4
  , 1
  , 0
  , 0
  , 0
  , 5
  , 6
  , 4
  , 3
  , 2
  , 0
  , 0
  , 1
  , 4
  , 6
  , 3
  , 5
  , 5
  , 5
  , 5
  , 1
  , 5
  , 5
  , 3
  , 6
  , 4
  , 6
  , 6
  , 1
  , 0
  , 6
  ]

-- | Just some inputs made manually by Erik
inputs :: [Puzzle]
inputs = [input0, input1, input2, input3, input4, input5, input6]

-- | Assignment 'Puzzle' 1
assignment1 :: Puzzle
assignment1 =
  [ 6
  , 6
  , 2
  , 6
  , 5
  , 2
  , 4
  , 1
  , 1
  , 3
  , 2
  , 0
  , 1
  , 0
  , 3
  , 4
  , 1
  , 3
  , 2
  , 4
  , 6
  , 6
  , 5
  , 4
  , 1
  , 0
  , 4
  , 3
  , 2
  , 1
  , 1
  , 2
  , 5
  , 1
  , 3
  , 6
  , 0
  , 4
  , 5
  , 5
  , 5
  , 5
  , 4
  , 0
  , 2
  , 6
  , 0
  , 3
  , 6
  , 0
  , 5
  , 3
  , 4
  , 2
  , 0
  , 3
  ]

-- | Assignment 'Puzzle' 2
assignment2 :: Puzzle
assignment2 =
  [ 5
  , 4
  , 3
  , 6
  , 5
  , 3
  , 4
  , 6
  , 0
  , 6
  , 0
  , 1
  , 2
  , 3
  , 1
  , 1
  , 3
  , 2
  , 6
  , 5
  , 0
  , 4
  , 2
  , 0
  , 5
  , 3
  , 6
  , 2
  , 3
  , 2
  , 0
  , 6
  , 4
  , 0
  , 4
  , 1
  , 0
  , 0
  , 4
  , 1
  , 5
  , 2
  , 2
  , 4
  , 4
  , 1
  , 6
  , 5
  , 5
  , 5
  , 3
  , 6
  , 1
  , 2
  , 3
  , 1
  ]

-- | Assignment 'Puzzle' 3
assignment3 :: Puzzle
assignment3 =
  [ 4
  , 2
  , 5
  , 2
  , 6
  , 3
  , 5
  , 4
  , 5
  , 0
  , 4
  , 3
  , 1
  , 4
  , 1
  , 1
  , 1
  , 2
  , 3
  , 0
  , 2
  , 2
  , 2
  , 2
  , 1
  , 4
  , 0
  , 1
  , 3
  , 5
  , 6
  , 5
  , 4
  , 0
  , 6
  , 0
  , 3
  , 6
  , 6
  , 5
  , 4
  , 0
  , 1
  , 6
  , 4
  , 0
  , 3
  , 0
  , 6
  , 5
  , 3
  , 6
  , 2
  , 1
  , 5
  , 3
  ]

-- | An unsolvable 'Puzzle'
unsolvable :: Puzzle
unsolvable = replicate 56 0
