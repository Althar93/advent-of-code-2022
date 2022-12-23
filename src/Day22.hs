module Day22 (day22Solver) where

import Common
import Parser
import Data.Maybe

-- The input file path
inputFile :: FilePath
inputFile = "res/day22_test_input.txt"

-- A direction, starting from East
data Direction = East | South | West | North deriving (Show, Enum)

-- A position
type Position = (Int, Int, Direction)

-- A move
data Move = Move Int | TurnLeft | TurnRight deriving (Show)

-- The map type consisting of the map itself & a path to follow
type Map = ([(Int, [Char])], [(Int, Int)])

-- A move function
type MoveFunction = Map -> Position -> (Int, Int) -> [Position]

-- Parses moves
parseMoves :: Parser [Move]
parseMoves = some $ parseMove <|> parseTurn where
    parseMove = do 
        n <- parseInt
        return $ Move n
    parseTurn = do
        c <- parseItem
        case c of
            'R'         -> return $ TurnRight
            'L'         -> return $ TurnLeft
            otherwise   -> error $ "Unrecognised move '" ++ [c] ++ "'"

-- Parses the map of the board
parseMapOfTheBoard :: Parser [(Int, [Char])]
parseMapOfTheBoard = some parseRow where
    parseRow = do
        o  <- many $ parseChar ' '
        rs <- some $ parseIsOneOf ".#"
        parseLineReturn
        return $ (length o, rs)

-- Parses a map
parseMapAndMoves :: Parser (Map, [Move])
parseMapAndMoves = do
    xs <- parseMapOfTheBoard
    parseSpaces
    ms <- parseMoves
    return $ ((xs, buildYWrapMap xs), ms)

-- Builds a wrap map for the Y coordinate
buildYWrapMap :: [(Int, [Char])] -> [(Int, Int)]
buildYWrapMap xs = [(findMinY x xs, findMaxY x xs) | x <- [0..maxX - 1]] where
    maxX = maximum $ map (\(o, rs) -> o + length rs) xs
    -- Min Y
    findMinY :: Int -> [(Int, [Char])] -> Int
    findMinY x xs = findMinY' 0 x xs where
        findMinY' n _ []            = n
        findMinY' n x ((o, rs):xs)  = if x - o >= 0 && x - o <= (length rs - 1) then n else findMinY' (n + 1) x xs
    -- Max Y
    findMaxY :: Int -> [(Int, [Char])] -> Int
    findMaxY x xs = findMaxY' (length xs - 1) x (reverse xs) where
        findMaxY' n _ []            = n
        findMaxY' n x ((o, rs):xs)  = if x - o >= 0 && x - o <= (length rs - 1) then n else findMaxY' (n - 1) x xs

-- Reads the test input
readInputs :: IO (Map, [Move])
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseMapAndMoves contents)

-- Draws the map
drawMap :: Map -> IO ()
drawMap m = mapM_ drawRows [0..length rs - 1] where
    rs = (fst m)
    drawRows y = putStrLn $ (\(o, rs) -> (take o (repeat ' ')) ++ rs) (rs !! y)

-- Executes a single move
executeMove :: MoveFunction -> Map -> Position -> Move -> [Position]
executeMove mf m (x, y, d) mv = case mv of 
    TurnLeft  -> case d of
        East        -> [(x, y, North)]
        otherwise   -> [(x, y, pred d)]
    TurnRight -> case d of
        North       -> [(x, y, East)]
        otherwise   -> [(x, y, succ d)]
    (Move n)  -> case d of
        North   -> mf m (x, y, d) ( 0,-n)
        East    -> mf m (x, y, d) ( n, 0)
        South   -> mf m (x, y, d) ( 0, n)
        West    -> mf m (x, y, d) (-n, 0)

-- Move function for part 1
movePart1 :: Map -> Position -> (Int, Int) -> [Position]
movePart1 m (x0, y0, d) (dx, dy)    | (dy == 0) = map fromJust $ takeWhile isJust (map checkCollisionX [(x0 + o0 + n * (signum dx), y0, d) | n <- [1..abs(dx)] ])
                                    | (dx == 0) = map fromJust $ takeWhile isJust (map checkCollisionY [(x0 + o0, y0 + n * (signum dy), d) | n <- [1..abs(dy)] ])
                                    | otherwise  = error $ "Diagonal movement not supported"
                                    where 
                                        (o0, _) = (fst m) !! y0
                                        -- Horizontal collision 
                                        checkCollisionX (x, y, d)  = case rs !! x' of
                                            '#' -> Nothing
                                            '.' -> Just (x', y, d)
                                            where
                                                x'              = (x - o) `mod` (length rs)
                                                (o, rs)         = (fst m) !! y
                                        -- Vertical collision ; we also account for Y wrap-around & offset between sections
                                        checkCollisionY (x, y, d)  = case rs !! x' of
                                            '#' -> Nothing
                                            '.' -> Just (x', y', d)
                                            where
                                                x'              = (x - o) `mod` (length rs)
                                                (o, rs)         = (fst m) !! y'
                                                y'              = ((y - minY) `mod` (maxY - minY + 1)) + minY
                                                (minY, maxY)    = (snd m) !! x

-- Move function for part 2 - assumes we are on a cube
movePart2 :: Int -> Map -> Position -> (Int, Int) -> [Position]
movePart2 s m (x0, y0, d) (dx, dy)    | (dy == 0) = map fromJust $ takeWhile isJust (map checkCollisionX [(x0 + o0 + n * (signum dx), y0, d) | n <- [1..abs(dx)] ])
                                    | (dx == 0) = map fromJust $ takeWhile isJust (map checkCollisionY [(x0 + o0, y0 + n * (signum dy), d) | n <- [1..abs(dy)] ])
                                    | otherwise  = error $ "Diagonal movement not supported"
                                    where 
                                        (o0, _) = (fst m) !! y0
                                        -- Horizontal collision 
                                        checkCollisionX (x, y, d)  = case rs !! x' of
                                            '#' -> Nothing
                                            '.' -> Just (x', y, d)
                                            where
                                                x'              = (x - o) `mod` (length rs)
                                                (o, rs)         = (fst m) !! y
                                        -- Vertical collision ; we also account for Y wrap-around & offset between sections
                                        checkCollisionY (x, y, d)  = case rs !! x' of
                                            '#' -> Nothing
                                            '.' -> Just (x', y', d)
                                            where
                                                x'              = (x - o) `mod` (length rs)
                                                (o, rs)         = (fst m) !! y'
                                                y'              = ((y - minY) `mod` (maxY - minY + 1)) + minY
                                                (minY, maxY)    = (snd m) !! x

-- Executes the sequence of moves
executeMoves :: MoveFunction -> Map -> Position -> [Move] -> [Position]
executeMoves mf mp p0 ms = foldl (\ps m -> ps ++ (executeMove mf mp (last ps) m)) [p0] ms 

-- Computes the password from the given position
computePassword :: Position -> Int
computePassword (x, y, d) = 1000 * (y + 1) + 4 * (x + 1) + (fromEnum d)

-- The solver for part #1 of the puzzle
solvePart1 :: (Map, [Move]) -> IO Int
solvePart1 (m, ms) = do
    let p0          = (0, 0, East)
    let ps          = executeMoves movePart1 m p0 ms
    let password    = computePassword (last ps)
    return $ password

-- The solver for part #2 of the puzzle
solvePart2 :: (Map, [Move]) -> IO Int
solvePart2 (m, ms) = do
    let p0          = (0, 0, East)
    let ps          = executeMoves (movePart2 4) m p0 ms
    let password    = computePassword (last ps)
    return $ password

-- The full solver
day22Solver :: IO [Int]
day22Solver = do
    input <- readInputs
    putStrLn $ show input

    part1 <- solvePart1 input
    part2 <- solvePart2 input

    --let (m, ms) = input
    --let ms' = [Move 4]
    --let p0 = (5, 7, South)
    --let p0 = (0, 0, South)
    --let ps = executeMoves m p0 ms'---executeMove input p0 m
    --putStrLn $ show p0

    --putStrLn $ show ps

    return [part1, part2]
