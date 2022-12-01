module Day1 (day1Solver) where

import Parser
import Data.List

-- The input file path
inputFile :: FilePath
inputFile = "res/day1_input.txt"

-- A basic elf
type Elf = [Int]

-- Parse for an Elf from a stream of characters
parseElf :: Parser Elf
parseElf = do
    xs <- some (pLine parseInt)
    pMaybe parseLineReturn
    return xs

-- Parse many elves
parseElves :: Parser [Elf]
parseElves = many parseElf

-- Reads the test input
readInputs :: IO [Elf]
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseElves contents

-- Just a simple test input
testInput :: [Elf]
testInput = [[1,2,3],[4,5,6]]

-- The solver for part #1 of the puzzle
solvePart1 :: [Elf] -> Int
solvePart1 xs = maximum $ map sum xs

-- The solver for part #2 of the puzzle
solvePart2 :: [Elf] -> Int
solvePart2 xs = sum $ take 3 (reverse (sort (map sum xs)))

-- The full solver
day1Solver :: IO [Int]
day1Solver = do
    --let input = testInput
    input <- readInputs
    putStrLn $ show input
    return [solvePart1 input, solvePart2 input]
