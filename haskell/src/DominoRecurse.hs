module DominoRecurse where

type Position = (Int, Int)

type Pips = (Int, Int)

type Bone = (Pips, Int)

dominoRecurse :: IO ()
dominoRecurse = do
  putStrLn "Domino Recurse"
  let solutions = recurse input0 initialPositions initialBones [initialSolution input0]
  print solutions
  return ()

maxPips :: Int
maxPips = 0

input0 :: [Int]
input0 = [0, 0]

input1 :: [Int]
input1 = [0, 0, 0, 1, 1, 1]

initialBones :: [Bone]
initialBones = funInitialBones maxPips

funInitialBones :: Int -> [Bone]
funInitialBones maxPips =
  zip [(pips1, pips2) | pips1 <- [0 .. maxPips], pips2 <- [pips1 .. maxPips]] [1 ..]

initialPositions :: [Position]
initialPositions = funInitialPositions maxPips

funInitialPositions :: Int -> [Position]
funInitialPositions maxPips =
  [(r, c) | r <- [1 .. funMaxRow maxPips], c <- [1 .. funMaxColumn maxPips]]

initialSolution :: [Int] -> [Int]
initialSolution = map (const 0)

maxRow :: Int
maxRow = funMaxRow maxPips

funMaxRow :: Int -> Int
funMaxRow maxPips = maxPips + 1

maxColumn :: Int
maxColumn = funMaxColumn maxPips

funMaxColumn :: Int -> Int
funMaxColumn maxPips = funMaxRow maxPips + 1

numBones :: Int
numBones = sum [1 .. maxPips + 1]

recurse ::
     [Int] -- ^ Numbers to place bones on
  -> [Position] -- ^ Set of available positions
  -> [Bone] -- ^ Set of available bones
  -> [[Int]] -- ^ Current solutions
  -> [[Int]] -- ^ Solutions
recurse _ [] [] solutions = solutions -- All bones have been positioned
recurse _ [] _ _ = [] -- No more available positions
recurse _ _ [] _ = [] -- No more available bones
recurse (pips:pipss) (pos:poss) bones (sol:sols) =
  let pipBones = bonesWithPips pips bones
      neighbours = neighboursInSet pos poss
  in []

bonesWithPips ::
     Int -- ^ 'Pips' to find
  -> [Bone] -- ^ Available 'Bone' instances
  -> [Bone]
bonesWithPips pips bones =
  [ ((pips3, pips4), num)
  | ((pips1, pips2), num) <- bones
  , pipsOnBone pips ((pips1, pips2), num)
  , let pips3 = pips
  , let pips4 =
          if pips == pips1
            then pips2
            else pips1
  ]

pipsOnBone :: Int -> Bone -> Bool
pipsOnBone pips ((pips1, pips2), _) = pips `elem` [pips1, pips2]

{-| Get the east (right) and south (down) neighbours of the specified position from the specified
    set of positions
-}
neighboursInSet :: Position -> [Position] -> [Position]
neighboursInSet (r, c) poss = [pos | pos <- [(r + 1, c), (r, c + 1)], pos `elem` poss]
