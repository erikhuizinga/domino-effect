module DominoRecurse where

dominoRecurse :: IO ()
dominoRecurse = do
  putStrLn "Domino Recurse"
  solutions <- recurseIO input0
  print solutions
  return ()

maxPips :: Int
maxPips = 0

input0 = [0, 0]

input1 = [0, 0, 0, 1, 1, 1]

initialBones :: [((Int, Int), Int)]
initialBones = zip [(pips1, pips2) | pips1 <- [0 .. maxPips], pips2 <- [pips1 .. maxPips]] [1 ..]

recurseIO :: [Int] -> IO [Int]
recurseIO xs = return $ recurse xs

recurse :: [Int] -> [Int]
recurse (x:xs) = []
