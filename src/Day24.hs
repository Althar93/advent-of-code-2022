module Day24 (day24Solver) where

import Common
import Parser
import Data.Function
import Data.List
import System.Console.ANSI
import System.Sleep

-- A 2-dimensional vector
type Vector2 = (Int, Int)

-- A blizzard consiting of positions and velocities
data BlizzardMap = BlizzardMap { pos :: [Vector2], vel :: [Vector2], walls :: [Vector2], start :: Vector2, end :: Vector2, size :: Vector2 } deriving (Show)

-- The input file path
inputFile :: FilePath
inputFile = "res/day24_input.txt"

-- Parses many blizzard
parseBlizzardMap :: Parser BlizzardMap
parseBlizzardMap = do
    rss <- some $ parseRow
    let rss' = map (init . tail) ((init . tail) rss)
    --error $ show rss'
    return $ buildBlizzardMap (length (rss' !! 0)) (length rss') rss'
    where
        parseRow = do
            parseSpaces
            rs <- some $ parseIsOneOf "#<>^v."
            return $ rs

-- Builds a map of blizzards
buildBlizzardMap :: Int -> Int -> [[Char]] -> BlizzardMap
buildBlizzardMap w h []  = error $ "Cannot create blizzard map from an empty map"
buildBlizzardMap w h rss = BlizzardMap { pos = ps, vel = vs, walls = ws, start = (0, -1), end = (w - 1, h), size = (w, h) } 
    where
        ws       = buildWalls w h
        (ps, vs) = unzip $ buildBlizzardMapRow rss 0
        buildBlizzardMapRow []      _ = []
        buildBlizzardMapRow (r:rss) y = (buildBlizzardMapColumn r 0 y) ++ (buildBlizzardMapRow rss (y + 1))
        buildBlizzardMapColumn [] _ _ = []
        buildBlizzardMapColumn (c:cs) x y = case c of
            '>'         -> ((x, y), ( 1,  0)) : buildBlizzardMapColumn cs (x + 1) y
            '<'         -> ((x, y), (-1,  0)) : buildBlizzardMapColumn cs (x + 1) y
            '^'         -> ((x, y), ( 0, -1)) : buildBlizzardMapColumn cs (x + 1) y
            'v'         -> ((x, y), ( 0,  1)) : buildBlizzardMapColumn cs (x + 1) y
            otherwise   -> buildBlizzardMapColumn cs (x + 1) y

-- Builds the wall cells of the blizzard map
buildWalls :: Int -> Int -> [Vector2]
buildWalls w h = lb ++ rb ++ bl ++ tr ++ es where
    lb     = [(-1,  y) | y <- [-1,0..h]]
    rb     = [( w,  y) | y <- [-1,0..h]]
    bl     = [( x,  h) | x <- [-1,0..w-2]]
    tr     = [( x, -1) | x <- [ 1..w]]
    es     = [( 0, -2), (w - 1, h + 1)]

-- Steps the blizzard once
step :: BlizzardMap -> BlizzardMap
step b = b { pos = p' } 
    where
        (w, h)  = size b
        p'      = zipWith (\(x, y) (dx, dy) -> ((x + dx) `mod` w, (y + dy) `mod` h)) (pos b) (vel b)

-- Steps the blizzard a number of times
stepN :: Int -> BlizzardMap -> BlizzardMap
stepN n b = foldr (\_ -> step) b [0..n]

-- Steps the blizzard a number of times (IO variant)
stepNIO :: Int -> BlizzardMap -> IO BlizzardMap
stepNIO 0 b = return $ b
stepNIO n b = do
    let b' = step b
    setCursorPosition 0 0
    drawMapASCII b'
    sleep 0.5
    stepNIO (n - 1) b'

-- Reads the test input
readInputs :: IO BlizzardMap
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseBlizzardMap contents)

-- Draws the elves
drawMap :: BlizzardMap -> IO ()
drawMap b = mapM_ drawRows [-1,0..h] where
    (w, h) = size b
    drawRows y = putStrLn $ map (drawCell y) [-1,0..w]
    drawCell y x | (x, y) == (0, -1)                = '.'
                 | (x, y) == (w - 1, h)             = '.'
                 | x < 0 || x > (w - 1) || y < 0 || y > (h - 1) = '#'
                 | otherwise                        = case elemIndices (x, y) (pos b) of 
                                                        []      -> '.'
                                                        [i]     -> case (vel b !! i) of
                                                            (-1,  0) -> '<'
                                                            ( 1,  0) -> '>'
                                                            ( 0, -1) -> '^'
                                                            ( 0,  1) -> 'v'
                                                        is      -> head $ show (length is)
            
-- Draws the elves with fancier ASCII graphics                                              
drawMapASCII :: BlizzardMap -> IO ()
drawMapASCII b = mapM_ drawRows [-1,0..h] where
    (w, h) = size b
    drawRows y = putStrLn $ map (drawCell y) [-1,0..w]
    drawCell y x | (x, y) == (0, -1)                = '#'
                 | (x, y) == (w - 1, h)             = '#'
                 | x < 0 || x > (w - 1) || y < 0 || y > (h - 1) = '█'
                 | otherwise                        = case elemIndices (x, y) (pos b) of 
                                                        []      -> ' '
                                                        [i]     -> case (vel b !! i) of
                                                            (-1,  0) -> '<'
                                                            ( 1,  0) -> '>'
                                                            ( 0, -1) -> '^'
                                                            ( 0,  1) -> 'v'
                                                        is      -> case (length is) of
                                                            2           -> '░'
                                                            3           -> '▒'
                                                            otherwise   -> '▓'

-- Returns the empty spaces around the specified position
emptySpaces :: BlizzardMap -> Vector2 -> Maybe [Vector2]
emptySpaces b (x, y) = case filter (not . (flip elem blockers)) [ (x + dx, y + dy) | (dx, dy) <- [(-1, 0), (0, -1), (1, 0), (0, 1), (0, 0)] ] of
    [] -> Nothing
    xs -> Just xs
    where
        blockers = (walls b)++(pos b)

-- Traces the expedition from start to finish and returns the shortest possible path (including waits)
traceExpeditionBFS :: BlizzardMap -> Vector2 -> Vector2 -> ([Vector2], BlizzardMap)
traceExpeditionBFS b0 s e = traceExpeditionBFS' b0 0 [[s]] where
    traceExpeditionBFS' :: BlizzardMap -> Int -> [[Vector2]] -> ([Vector2], BlizzardMap)
    traceExpeditionBFS' b n pss     | null pss                  = error $ "No paths could be found"
                                    | any ((==e) . head) pss    = (mpss, b)
                                    | otherwise                 = traceExpeditionBFS' b' (n+1) pss'''
                                    where
                                        -- Shortest path that reaches the goal
                                        mpss = minimumBy (compare `on` length) (filter ((==e) . head) pss)
                                        -- Step blizzard
                                        b'      = step b
                                        -- Remove any that end up in the same spot
                                        pss'''  = map head pss'' 
                                        pss''   = groupBy (\a b -> (head a) == (head b)) (sort pss')
                                        -- Step all positions
                                        pss'    = concatMap (traceSingle b') pss
                                        -- Single trace
                                        traceSingle :: BlizzardMap -> [Vector2] -> [[Vector2]]
                                        traceSingle b (p:ps) | p == e       = [(p:ps)]
                                                             | otherwise    = case (emptySpaces b p) of
                                                                Nothing -> []
                                                                Just es -> map (\p' -> (p':p:ps)) es

-- The solver for part #1 of the puzzle
solvePart1 :: BlizzardMap -> Int
solvePart1 b = length ts - 1 where
    (ts, _) = traceExpeditionBFS b (start b) (end b)

-- The solver for part #2 of the puzzle
solvePart2 :: BlizzardMap -> Int
solvePart2 b = (length ts - 1) + (length ts' - 1) + (length ts'' - 1) where
    (ts'', _  )  = traceExpeditionBFS b''    (start b)   (end b)
    (ts' , b'')  = traceExpeditionBFS b'     (end b)     (start b)
    (ts  , b' )  = traceExpeditionBFS b      (start b)   (end b)

-- The full solver
day24Solver :: IO [Int]
day24Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
