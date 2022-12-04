module Day4 (day4Solver) where

import Parser
import Common

-- The input file path
inputFile :: FilePath
inputFile = "res/day4_input.txt"

-- A pair consisting of two sections
type Pair = ([Int],[Int])

-- Parses a single rucksack
parsePair :: Parser Pair
parsePair = do
    x0 <- parseUnsignedInt
    parseChar '-'
    x1 <- parseUnsignedInt
    parseChar ','
    y0 <- parseUnsignedInt
    parseChar '-'
    y1 <- parseUnsignedInt
    pMaybe parseLineReturn
    --let halfLength = length contents `div` 2
    return ([x0..x1], [y0..y1])

-- Parses all the pairs
parsePairs :: Parser [Pair]
parsePairs = many parsePair

-- Reads the test input
readInputs :: IO [Pair]
readInputs = do
    contents <- readFile inputFile
    return $ runParser parsePairs contents

-- The solver for part #1 of the puzzle
solvePart1 :: [Pair] -> Int
solvePart1 xss = length $ filter (\(xs,ys) -> xs `fullyOverlaps ` ys || ys `fullyOverlaps ` xs) xss where
    fullyOverlaps = all . (flip elem)

-- The solver for part #2 of the puzzle
solvePart2 :: [Pair] -> Int
solvePart2 xss = length $ filter (\(xs,ys) -> xs `hasOverlap` ys || ys `hasOverlap` xs) xss where 
    hasOverlap = any . (flip elem)

-- The full solver
day4Solver :: IO [Int]
day4Solver = do
    input <- readInputs
    putStrLn $ show input
    return [solvePart1 input, solvePart2 input]