module Day5 (day5Solver) where

import Parser
import Common
import Data.Maybe
import Data.Char

-- The input file path
inputFile :: FilePath
inputFile = "res/day5_input.txt"

-- A puzzle
data Puzzle = Puzzle {stacks :: [Stack], moves :: [Move] } deriving (Show)

-- A single stack
type Stack = [Char]

-- A single move
type Move = (Int, Int, Int)

-- Parses a single move
parseMove :: Parser Move
parseMove = do
    parseString "move"
    parseSpaces
    n <- parseInt
    parseSpaces
    parseString "from"
    parseSpaces
    f <- parseInt
    parseSpaces
    parseString "to"
    parseSpaces
    t <- parseInt
    pMaybe parseLineReturn
    return (n, f, t)

-- Parses a single cell
parseCell :: Parser String
parseCell = do
    parseIsOneOf "[ "
    c <- parseItem
    parseIsOneOf "] "
    parseItem
    return $ if c == ' ' then [] else [c]

-- Parses rows into a flat list of cells
parseRows :: Parser [String]
parseRows = many parseCell
 
-- Parses the stack
parseStacks :: Parser [Stack]
parseStacks = do
    rs <- parseRows
    let n   = read (rs !! ((length rs) - 1))
    let rs' = init (chunksOf n rs)
    let ss  = foldr (zipWith (++)) (repeat "") rs'
    return ss

-- Parses the puzzle
parsePuzzle :: Parser Puzzle
parsePuzzle = do
    ss <- parseStacks
    parseSpaces
    ms <- many parseMove
    return Puzzle { stacks = ss, moves = ms }

-- Reads the test input
readInputs :: IO Puzzle
readInputs = do
    contents <- readFile inputFile
    return $ runParser parsePuzzle contents

-- Single step a stack
stepStack :: Move -> Bool -> [Stack] -> [Stack]
stepStack m r sss                           = stepStack' m r 1 sss where
    stepStack' _ _ _ []                     = []
    stepStack' m r i (s:ss)                 = (updateStack m r i s) : (stepStack' m r (i + 1) ss)
    updateStack (n, f, t) r i s | (i == t)  = if r then (reverse (take n (sss !! (f - 1)))) ++ s else (take n (sss !! (f - 1))) ++ s
                                | (i == f)  = drop n s
                                | otherwise = s

-- Runs the entire puzzle - the boolean indicates whether we 
-- should be using 'CrateMover 9000' which can only pick one crate at a time (reversing the order)
-- or 'CrateMover 9001' which can pick creates and retains the order
runPuzzle :: Puzzle -> Bool -> [Stack]
runPuzzle Puzzle { stacks = s, moves = [] } r = s
runPuzzle Puzzle { stacks = s, moves = m  } r = runPuzzle Puzzle { stacks = s', moves = m' } r where
    s' = stepStack (head m) r s
    m' = tail m

-- The solver for part #1 of the puzzle
solvePart1 :: Puzzle -> String
solvePart1 xss = map head (runPuzzle xss True)

-- The solver for part #2 of the puzzle
solvePart2 :: Puzzle -> String
solvePart2 xss = map head (runPuzzle xss False)

-- The full solver
day5Solver :: IO [String]
day5Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
