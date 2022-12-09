module Day9 (day9Solver) where

import Common
import Parser
import Data.Char
import Data.List

-- The input file path
inputFile :: FilePath
inputFile = "res/day9_input.txt"

-- A position
type Vector2 = (Int, Int)

-- A move
type Move = Vector2

-- A rope
type Rope = [Vector2]

-- Makes a rope with the head and tail overlapping
mkRope :: Int -> Rope
mkRope n = take n (repeat (0, 0))

-- Parses a single move
parseMove :: Parser [Move]
parseMove = do
    d <- parseIsOneOf "UDLR"
    parseSpaces
    n <- parseInt
    case d of
        'U' -> return $ take n (repeat ( 0, -1))
        'D' -> return $ take n (repeat ( 0,  1))
        'L' -> return $ take n (repeat (-1,  0))
        'R' -> return $ take n (repeat ( 1,  0))

-- Parses moves
parseMoves :: Parser [Move]
parseMoves = do
    ms <- many $ pLine parseMove
    return $ concat ms

-- Reads the test input
readInputs :: IO [Move]
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseMoves contents)

-- Constrains
constrain :: Vector2 -> Vector2 -> Vector2
constrain (xt, yt) (xh, yh) | (xt == xh)        && abs(yh - yt) >  1 = (xt, yt + signum(yh - yt))
                            | (yt == yh)        && abs(xh - xt) >  1 = (xt + signum(xh - xt), yt)
                            | abs(yt - yh) <= 1 && abs(xt - xh) <= 1 = (xt, yt)
                            | otherwise                              = (xt + signum(xh - xt), yt + signum(yh - yt))

-- Executes a single move of the rope and constrains it
executeMove :: Rope -> Move -> Rope
executeMove ((xh, yh):rs) (u, v) = (xh', yh'):constrain' (xh', yh') rs where
    (xh', yh')                          = (xh + u, yh + v)
    constrain' (x0, y0) []              = []
    constrain' (x0, y0) ((x, y):rs)     = (x', y'):(constrain' (x', y') rs) where
        (x', y') = constrain (x, y) (x0, y0)

-- Simulate the motion
simulateMotion :: [Move] -> Rope -> Rope
simulateMotion xs r = foldl executeMove r xs

-- Simulate the motion and track the positions
simulateMotionAndTrack :: [Move] -> Rope -> (Rope, [Vector2])
simulateMotionAndTrack xs r = foldl executeMoveAndTrack (r, []) xs where
    executeMoveAndTrack (r, xs) m = (r', xs ++ [last r']) where
        r' = executeMove r m

-- The solver for part #1 of the puzzle
solvePart1 :: [Move] -> Int
solvePart1 xs = length $ removeDuplicates (snd (simulateMotionAndTrack xs (mkRope 2)))

-- The solver for part #2 of the puzzle
solvePart2 :: [Move] -> Int
solvePart2 xs = length $ removeDuplicates (snd (simulateMotionAndTrack xs (mkRope 10)))

-- The full solver
day9Solver :: IO [Int]
day9Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
