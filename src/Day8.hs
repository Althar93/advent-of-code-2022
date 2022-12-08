module Day8 (day8Solver) where

import Parser
import Data.Char
import Data.List

-- The input file path
inputFile :: FilePath
inputFile = "res/day8_input.txt"

-- A forest
type Forest = [[Int]]

-- Parses a tree
parseTree :: Parser Int
parseTree = do 
  c <- parseDigit
  return $ read [c]

-- Parses a forest
parseForest :: Parser Forest
parseForest = some $ (pLine (some parseTree))
  
-- Reads the test input
readInputs :: IO Forest
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseForest contents)

-- A variant of take while which also retains the element the moment when the condition fails
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []     = []
takeWhile' p (x:xs) 
        | p x       = x : takeWhile' p xs
        | otherwise = [x]

-- Computes the visibility of a tree
computeVisibility :: Int -> Int -> Forest -> Bool
computeVisibility x y fs    | x == 0 || x == (length (fs !! 0) - 1) = True
                            | y == 0 || y == (length fs - 1)        = True
                            | otherwise                             = any (< hCur) [mLeft, mRight, mTop, mBottom] where
                                hCur    = ((fs !! y) !! x)
                                mLeft   = maximum [ ((fs !! y)  !! x') | x' <- (reverse [0..x - 1])             ]
                                mRight  = maximum [ ((fs !! y)  !! x') | x' <- [x + 1..(length (fs !! 0) -1)]   ]
                                mTop    = maximum [ ((fs !! y') !! x ) | y' <- (reverse [0..y - 1])             ]
                                mBottom = maximum [ ((fs !! y') !! x ) | y' <- [y + 1..(length fs - 1)]         ]

-- Computes the scenic score of a tree
computeScenicScore :: Int -> Int -> Forest -> Int
computeScenicScore x y fs   | x == 0 || x == (length (fs !! 0) - 1) = 0
                            | y == 0 || y == (length fs - 1)        = 0 
                            | otherwise                             = sLeft * sRight * sTop * sBottom where
                                hCur    = ((fs !! y) !! x)
                                sLeft   = length $ takeWhile' (< hCur) [ ((fs !! y)  !! x') | x' <- (reverse [0..x - 1])             ]
                                sRight  = length $ takeWhile' (< hCur) [ ((fs !! y)  !! x') | x' <- [x + 1..(length (fs !! 0) -1)]   ]
                                sTop    = length $ takeWhile' (< hCur) [ ((fs !! y') !! x ) | y' <- (reverse [0..y - 1])             ]
                                sBottom = length $ takeWhile' (< hCur) [ ((fs !! y') !! x ) | y' <- [y + 1..(length fs - 1)]         ]

-- Traverses a forest & maps the function
traverseForest :: (Int -> Int -> Forest -> a) -> Forest -> [[a]]
traverseForest f fs = [ [ (f x y fs) | x <- [0..(length (fs !! 0) - 1)] ] | y <- [0..(length fs - 1)] ]

-- Tree visibility
mapTreeVisibility :: Forest -> [[Bool]]
mapTreeVisibility fs = traverseForest computeVisibility fs

-- Tree scenic score
mapTreeScenicScore :: Forest -> [[Int]]
mapTreeScenicScore fs = traverseForest computeScenicScore fs

-- The solver for part #1 of the puzzle
solvePart1 :: Forest -> Int
solvePart1 fs = sum $ map fromEnum (concat (mapTreeVisibility fs))

-- The solver for part #2 of the puzzle
solvePart2 :: Forest -> Int
solvePart2 fs = maximum $ concat (mapTreeScenicScore fs) 

-- The full solver
day8Solver :: IO [Int]
day8Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]