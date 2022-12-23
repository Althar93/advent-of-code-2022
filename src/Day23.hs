module Day23 (day23Solver) where

import Common
import Parser
import Data.Maybe
import Data.List

-- The cardinla directions
data Direction = NW | NE | SE | SW | N | E | S | W deriving (Show, Enum)

-- An elf defined by a 2D position
type Elf = (Int, Int)

-- The input file path
inputFile :: FilePath
inputFile = "res/day23_input.txt"

-- Parses elves
parseElves :: Parser [Elf]
parseElves = do 
    es <- some $ parseRow
    return $ buildElfList es
    where
        parseRow = do
            parseSpaces
            xs <- some $ parseIsOneOf ".#"
            return xs

-- Builds a list of elves from a map of characters
buildElfList :: [[Char]] -> [Elf]
buildElfList xs = buildElfListRow xs 0 where
    buildElfListRow []      _       = []
    buildElfListRow (r:rs)  y       = (buildElfListColumn r 0 y) ++ (buildElfListRow rs (y+1))
    buildElfListColumn [] _ _       = []
    buildElfListColumn (c:cs) x y   = case c of
        '#'         -> (x, y) : buildElfListColumn (cs) (x+1) y 
        otherwise   -> buildElfListColumn (cs) (x+1) y 

-- Reads the test input
readInputs :: IO [Elf]
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseElves contents)

-- Returns the neighbours of a given elf
neighbours :: [Elf] -> Elf -> [Maybe Elf]
neighbours es (x, y) = map (\n -> if n `elem` es then Just n else Nothing) ns where
    ns = map (\(dx, dy) -> (x + dx, y + dy)) [(-1, -1), (1, -1), (1, 1), (-1, 1), (0, -1), (1, 0), (0, 1), (-1, 0)]--[(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)]

-- First half of a round
stepFirstHalf :: [Elf] -> Int -> [Maybe Elf] 
stepFirstHalf es0 n = stepFirstHalf' es0 where
    -- Internal step
    stepFirstHalf' :: [Elf] -> [Maybe Elf]
    stepFirstHalf' []       = []
    stepFirstHalf' (e:es)   = (selectFreePosition (neighbours es0 e) e) : (stepFirstHalf' es)
    -- Select where to go
    selectFreePosition :: [Maybe Elf] -> Elf -> Maybe Elf
    selectFreePosition ns (x, y)    | all isNothing ns = Nothing 
                                    | otherwise        = case find isJust (rotate n options) of 
                                        Nothing -> Nothing
                                        Just x  -> x 
                                        where 
                                            options = [ if all (\d -> ns' !! (fromEnum d)) [NW, N, NE] then Just (x, y - 1) else Nothing,
                                                        if all (\d -> ns' !! (fromEnum d)) [SW, S, SE] then Just (x, y + 1) else Nothing,
                                                        if all (\d -> ns' !! (fromEnum d)) [SW, W, NW] then Just (x - 1, y) else Nothing,
                                                        if all (\d -> ns' !! (fromEnum d)) [NE, E, SE] then Just (x + 1, y) else Nothing ]
                                            ns' = map isNothing ns

-- Second half of a round
stepSecondHalf :: [Elf] -> [Maybe Elf] -> [Elf]
stepSecondHalf es0 ms0 = stepSecondHalf' es0 ms0 where
    -- Internal step
    stepSecondHalf' :: [Elf] -> [Maybe Elf] -> [Elf]
    stepSecondHalf' [] []           = []
    stepSecondHalf' (e:es) (m:ms)   = case isUnique m ms0 of
        Just True -> case m of 
            Nothing -> e:(stepSecondHalf' es ms)
            Just e' -> e':(stepSecondHalf' es ms)
        otherwise -> e:(stepSecondHalf' es ms)

-- Executes a full step
step :: [Elf] -> Int -> [Elf]
step es n = es' where
    es' = stepSecondHalf es ms 
    ms  = stepFirstHalf es n

-- Executes N steps
stepN :: [Elf] -> Int -> [Elf]
stepN es n = foldl step es [0..n-1]

-- Executes steps until no elf is found to have moved
stepUntilNoMove :: [Elf] -> (Int, [Elf])
stepUntilNoMove es = stepUntilNoMove' es 0 where
    stepUntilNoMove' es n   | es == es' = (n+1, es)
                            | otherwise = stepUntilNoMove' es' (n+1)
                            where
                                es' = step es n

-- Compute the number of empty tiles (i.e. not occupied by an elf)
emptyTiles :: [Elf] -> Int
emptyTiles es = (w * h) - (length es) where
    w  = 1 + (maximum xs) - (minimum xs)
    h  = 1 + (maximum ys) - (minimum ys)
    xs = map fst es
    ys = map snd es 

-- Draws the eleves
drawElves :: [Elf] -> IO ()
drawElves es = mapM_ drawRows [minimum ys - mg..maximum ys + mg] where
    mg = 2
    xs = map fst es
    ys = map snd es 
    drawRows y = putStrLn $ map (drawCell y) [minimum xs - mg..maximum xs + mg]
    drawCell y x | (x, y) `elem` es    = '#'
                 | otherwise           = '.'

-- The solver for part #1 of the puzzle
solvePart1 :: [Elf] -> IO Int
solvePart1 es = do
    let es' = stepN es 10
    putStrLn $ "Part 1 :"
    drawElves es'
    return $ emptyTiles es'

-- The solver for part #2 of the puzzle
solvePart2 :: [Elf] -> IO Int
solvePart2 es = do
    let (n, es') = stepUntilNoMove es
    putStrLn $ "Part 2 :"
    drawElves es'
    return $ n

-- The full solver
day23Solver :: IO [Int]
day23Solver = do
    input <- readInputs
    part1 <- solvePart1 input
    part2 <- solvePart2 input
    return [part1, part2]
