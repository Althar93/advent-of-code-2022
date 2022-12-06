module Day6 (day6Solver) where

import Data.Maybe

-- The input file path
inputFile :: FilePath
inputFile = "res/day6_input.txt"

-- Reads the test input
readInputs :: IO String
readInputs = do
    contents <- readFile inputFile
    return $ contents

-- Returns whether the list contains all unique elements
allUnique :: (Eq a) => [a] -> Bool
allUnique []            = True
allUnique (x:xs)        = (allUnique' x xs) && (allUnique xs) where
    allUnique' _ []     = True
    allUnique' x (y:ys) = if x == y then False else allUnique' x ys

-- Finds a marker
findMarker :: Int -> String -> Maybe (String, Int)
findMarker s xs = findMarker' s 0 xs where
    findMarker' s n xs  | length xs < s         = Nothing
                        | allUnique (take s xs) = Just ((take s xs), (n + s))
                        | otherwise             = findMarker' s (n + 1) (tail xs)

-- The solver for part #1 of the puzzle
solvePart1 :: String -> (String, Int)
solvePart1 xss = fromJust $ findMarker 4 xss

-- The solver for part #2 of the puzzle
solvePart2 :: String -> (String, Int)
solvePart2 xss = fromJust $ findMarker 14 xss

-- The full solver
day6Solver :: IO [(String, Int)]
day6Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
