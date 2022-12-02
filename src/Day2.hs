module Day2 (day2Solver) where

import Parser

-- The various moves
data Move = Rock | Paper | Scissors deriving (Show, Eq)

-- Outcome of a round
data Outcome = Win | Lose | Draw deriving (Show, Eq)

-- A single round
type Round = (Move, Move)

-- The input file path
inputFile :: FilePath
inputFile = "res/day2_input.txt"

-- Parses a single move
parseMove :: Parser Move
parseMove = do
    c <- parseItem
    if c == 'A' || c == 'X' then 
        return Rock
    else if c == 'B' || c == 'Y' then 
        return Paper
    else if c == 'C' || c == 'Z' then
        return Scissors
    else
        error "Unrecognised move"

-- Parses a single round
parseRound :: Parser Round
parseRound = do
    om <- parseMove
    parseSpaces
    pm <- parseMove
    pMaybe parseLineReturn
    return (om, pm)

-- Parse multiple rounds
parseRounds :: Parser [Round]
parseRounds = many parseRound

-- Reads the test input
readInputs :: IO [Round]
readInputs = do
    contents <- readFile inputFile
    return $ runParser parseRounds contents

-- Converts a move to an outcome
toOutcome :: Move -> Outcome
toOutcome Rock      = Lose
toOutcome Paper     = Draw
toOutcome Scissors  = Win

-- Plays a normal round
playRound :: Round -> Outcome
playRound (Scissors, Rock)         = Win
playRound (Rock, Paper)            = Win
playRound (Paper, Scissors)        = Win
playRound (om, pm)                 = if (om == pm) then Draw else Lose

-- Makes a round given an opponent's move & the desired outcome
mkRound :: Outcome -> Move -> Round
mkRound Draw  om        = (om, om)
mkRound Win   Scissors  = (Scissors,    Rock)
mkRound Win   Rock      = (Rock,        Paper)
mkRound Win   Paper     = (Paper,       Scissors)
mkRound Lose  Paper     = (Paper,       Rock)
mkRound Lose  Scissors  = (Scissors,    Paper)
mkRound Lose  Rock      = (Rock,        Scissors)

-- Scores a single move
scoreMove :: Move -> Int
scoreMove Rock      = 1
scoreMove Paper     = 2
scoreMove Scissors  = 3

-- Scores an outcome
scoreOutcome :: Outcome -> Int
scoreOutcome Win    = 6
scoreOutcome Draw   = 3
scoreOutcome Lose   = 0

-- Scores the strategy
scoreRound :: Round -> Int
scoreRound r@(_, pm) = (scoreOutcome (playRound r)) + (scoreMove pm)

-- The solver for part #1 of the puzzle
solvePart1 :: [Round] -> Int
solvePart1 xs = sum $ map scoreRound xs

-- The solver for part #2 of the puzzle
solvePart2 :: [Round] -> Int
solvePart2 xs = sum $ map scoreRound xs' where
    xs' = map (\(om, po) -> mkRound (toOutcome po) om) xs

-- The full solver
day2Solver :: IO [Int]
day2Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]