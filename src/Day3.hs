module Day3 (day3Solver) where

import Parser
import Common
import Data.Char

-- The input file path
inputFile :: FilePath
inputFile = "res/day3_input.txt"

-- A ruck sack, consisting of two compartments
type Rucksack = ([Char],[Char])

-- Parses a single rucksack
parseRucksSack :: Parser Rucksack
parseRucksSack = do
    contents <- some (parseIs isLetter)
    pMaybe parseLineReturn
    let halfLength = length contents `div` 2
    return (take halfLength contents, drop halfLength contents)

-- Parses all the ruck sacks
parseRucksSacks :: Parser [Rucksack]
parseRucksSacks = many parseRucksSack

-- Reads the test input
readInputs :: IO [Rucksack]
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseRucksSacks contents

-- Makes a rucksack
mkRuckSack :: [Char] -> [Char] -> Rucksack
mkRuckSack l r = (l, r)

-- Finds and returns the duplicate items from two lists
findDuplicateItems :: [Char] -> [Char] -> [Char]
findDuplicateItems xs ys = removeDuplicates $ filter (\x -> x `elem` ys ) xs 

-- Finds and returns the duplicate items from a list of rucksacks
findDuplicateItems' :: [[Char]] -> [Char]
findDuplicateItems' xs = foldl findDuplicateItems (head xs) (tail xs)

-- Computes the priority of a single ite,
computeItemPriority :: Char -> Int
computeItemPriority c   | isLower c = (fromEnum c) - (fromEnum 'a') + 1
                        | otherwise = (fromEnum c) - (fromEnum 'A') + 27


-- The solver for part #1 of the puzzle
solvePart1 :: [Rucksack] -> Int
solvePart1 xs = sum $ map computeRuckSackPriority xs where
    computeRuckSackPriority r = sum $ map computeItemPriority (findDuplicateItems (fst r) (snd r))

-- The solver for part #2 of the puzzle
solvePart2 :: [Rucksack] -> Int
solvePart2 xs = sum $ map computeItemPriority (concatMap findDuplicateItems' xs') where
    xs' = (chunksOf 3 (map (\(l, r) -> l++r) xs))

-- The full solver
day3Solver :: IO [Int]
day3Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]