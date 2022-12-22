module Day20 (day20Solver) where

import Common
import Parser
import Data.List
import Data.Maybe

-- The input file path
inputFile :: FilePath
inputFile = "res/day20_input.txt"

-- Parses a list of numbers
parseNumbers :: Parser [Int]
parseNumbers = do
    xs <- some $ pLine parseInt
    return xs

-- Reads the test input
readInputs :: IO [Int]
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseNumbers contents)

-- Delete a value from the list at the prescribed index
deleteAt :: Int -> [a] -> [a]
deleteAt n xs = ls ++ rs where
    (ls, (_:rs)) = splitAt n xs

-- Inserts the value at the prescribed index
insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs = ls ++ (x:rs) where
    (ls, rs) = splitAt n xs

-- Mixes the numbers a number of times
mix :: Int -> [Int] -> [Int]
mix n xs = map snd $ foldl mixElem ixs0 ixsN where
    ixsN = take (n * length ixs0) (cycle ixs0)
    ixs0 = zip [0..length xs - 1] xs
    mixElem ixs (k, x)  | x /= 0 && (i + x) `mod` (length xs - 1) == 0  = insertAt ((length xs - 1))               (k, x) ixs'
                        | otherwise                                     = insertAt ((i + x) `mod` (length xs - 1)) (k, x) ixs' 
                        where
                            ixs' = deleteAt i ixs
                            i    = fromJust $ findIndex ((==k) . fst) ixs

-- Computes the grove coordinate from a mixed list
groveCoordinate :: [Int] -> Int
groveCoordinate xs = case elemIndex 0 xs of
    Nothing     -> 0
    Just idx    -> t1 + t2 + t3 where
        t1  = xs !! ((idx + 1000) `mod` (length xs))
        t2  = xs !! ((idx + 2000) `mod` (length xs))
        t3  = xs !! ((idx + 3000) `mod` (length xs))
        idx = fromJust $ elemIndex 0 xs

-- The solver for part #1 of the puzzle
solvePart1 :: [Int] -> Int
solvePart1 xs = groveCoordinate xs' where
    xs' = mix 1 xs

-- The solver for part #2 of the puzzle
solvePart2 :: [Int] -> Int
solvePart2 xs = groveCoordinate xs'' where
    xs''    = mix 10 xs'
    xs'     = map (*811589153) xs

-- The full solver
day20Solver :: IO [Int]
day20Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
