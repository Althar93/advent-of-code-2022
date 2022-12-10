module Day10 (day10Solver) where

import Common
import Parser
import Data.Char
import Data.List

-- The input file path
inputFile :: FilePath
inputFile = "res/day10_input.txt"

-- A basic instruction
data Instruction = Noop | AddX Int deriving (Show)

--The program state, consisting of the X value and cycle count
type ProgramState = (Int, Int)

-- Makes a program state with X=1
mkProgramState :: ProgramState
mkProgramState = (1, 0)

-- parses an "addx" instruction
parseAddX :: Parser Instruction
parseAddX = do 
    parseString "addx" 
    parseSpaces
    n <- parseInt
    return $ AddX n

-- Parses a "noop" insutrction
parseNoop :: Parser Instruction
parseNoop = do
    parseString "noop"
    return $ Noop

-- Parse a single command
parseInstruction :: Parser Instruction
parseInstruction = parseAddX <|> parseNoop

-- Parse commands
parseInstructions :: Parser [Instruction]
parseInstructions = many $ pLine parseInstruction

-- Reads the test input
readInputs :: IO [Instruction]
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseInstructions contents)

executeProgramAndTrack :: [Instruction] -> ProgramState -> [ProgramState]
executeProgramAndTrack iss p = executeProgramAndTrack' iss p [] where
    executeProgramAndTrack' []         _        ss = ss
    executeProgramAndTrack' (i:is)     p@(x, c) ss = case i of 
        Noop   -> (executeProgramAndTrack' is (x, c + 1)        (ss ++ [(x, c + 1)])) 
        AddX n -> (executeProgramAndTrack' is (x + n, c + 2)    (ss ++ [(x, c + 1), (x, c + 2)])) 

executeProgramAndTrackFilter :: [Instruction] -> [Int] -> ProgramState -> [ProgramState]
executeProgramAndTrackFilter is fs p = filter (\(_, c) -> c `elem` fs) (executeProgramAndTrack is p)

-- Draws the CRT given a function of pixel position and cycle value
drawCRT :: Int -> Int -> (Int -> Int -> Bool) -> IO()
drawCRT w h f = mapM_ drawCRTRow [0..h-1] where
    drawCRTRow y = putStrLn $ map (cellToSymbol y) [0..w-1] where
        cellToSymbol y x    | f l c || f m c || f r c  = '█'
                            | otherwise                = '.' where
                                l = (x - 1)
                                m = x                    
                                r = (x + 1)
                                c = 1 + (x + y * w)

-- Compute the strength of the signal
computeSignalStrength :: ProgramState -> Int
computeSignalStrength (x, c) = x * c

-- The solver for part #1 of the puzzle
solvePart1 :: [Instruction] -> Int
solvePart1 is = sum $ map computeSignalStrength (executeProgramAndTrackFilter is [20, 60, 100, 140, 180, 220] mkProgramState)

-- The solver for part #2 of the puzzle
solvePart2 :: [Instruction] -> IO()
solvePart2 is = drawCRT w h (\p c -> (ps' !! (c - 1)) == p) where
    ps'             = map fst ps
    ps              = executeProgramAndTrackFilter is (take (w * h) [1..]) mkProgramState
    w               = 40
    h               = 6

-- The full solver
day10Solver :: IO [Int]
day10Solver = do
    input <- readInputs
    solvePart2 input
    return [solvePart1 input]
