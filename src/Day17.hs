module Day17 (day17Solver) where

import Common
import Parser
import Data.List
import Data.Maybe

-- The input file path
inputFile :: FilePath
inputFile = "res/day17_input.txt"

--A move
data Move = LeftMove | RightMove deriving (Show)

-- A simple shape
type Shape = [[Char]]

-- The tunnel
data Tunnel = Tunnel { localHeight :: Int, cachedHeight :: Int, noofRocks :: Int, rows :: [[Char]]}

-- Makes a brand new tunnel
mkTunnel :: Tunnel
mkTunnel = Tunnel { localHeight = 0, cachedHeight = 0, noofRocks = 0, rows = ("-------" : (repeat ".......")) }

rockA :: Shape
rockA = ["####"]

rockB :: Shape
rockB = [".#.", "###", ".#."]

rockC :: Shape
rockC = ["..#", "..#", "###"]

rockD :: Shape
rockD = ["#", "#", "#", "#"]

rockE :: Shape
rockE = ["##", "##"]

-- All rock types (reversed because Y increases from the bottom)
rocks :: [Shape]
rocks = map reverse [rockA, rockB, rockC, rockD, rockE]

maxHeight :: Tunnel -> Int
maxHeight t = (cachedHeight t) + (localHeight t)

-- Trims a tunnel 
trimTunnel :: Tunnel -> Tunnel
trimTunnel t    | (localHeight t) > 2 * mH = t { cachedHeight = c', localHeight = l', rows = r' }
                | otherwise               = t
                where
                    l' = (localHeight t) - mH
                    c' = (cachedHeight t) + mH                 
                    r' = drop mH (rows t)
                    mH = 1000000000

-- Splits a list into three parts
splitList :: Int -> Int -> [a] -> ([a], [a], [a])
splitList s e xs = (l, m, r) where
    l = take (s + 1) xs
    m = drop (s + 1) (take (e + 1) xs)
    r = drop (e + 1) xs

-- Makes rows
mkRows :: Shape -> Int -> Int -> [[Char]]
mkRows []     _ _ = []
mkRows (x:xs) d l = (left ++ x ++ right) : (mkRows xs d l) where
    left   = take d (repeat '.')
    right  = take (l - (length x + d)) (repeat '.')

-- Merge two rows together
mergeRow :: [Char] -> [Char] -> [Char]
mergeRow [] []          = []
mergeRow []  _          = error "Row should have matching dimensions"
mergeRow _  []          = error "Row should have matching dimensions"
mergeRow (x:xs) (y:ys)  | x == '#' || y == '#' = '#' : (mergeRow xs ys)
                        | otherwise            = '.' : (mergeRow xs ys)

-- Checks for row collisions
rowCollision :: [Char] -> [Char] -> Bool
rowCollision [] []          = False
rowCollision [] _           = True
rowCollision _  []          = True
rowCollision (x:xs) (y:ys)  | x /= '.' && y /= '.' = True
                            | otherwise            = rowCollision xs ys

-- Checks for collisions between a shape and the tunnel
checkCollision :: Shape -> Int -> Int -> Tunnel -> Bool
checkCollision s x y t = any (\(a, b) -> rowCollision a b) (zip ts ss) where
    (_, ts, _)  = splitList (y - (length s)) y (rows t)
    ss          = mkRows s x 7 

-- Pastes the shape in the list
pasteShape :: Shape -> Int -> Int -> [[Char]] -> [[Char]]
pasteShape s x y ys = bot ++ (map (\(a, b) -> mergeRow a b) (zip ss mid)) ++ top where
    (bot, mid, top) = splitList (y - (length ss)) y ys
    ss  = (mkRows s x 7)

-- Parses a single valve
parseMoves :: Parser [Move]
parseMoves = some $ parseLeft <|> parseRight where
    parseLeft   = do 
        parseChar '<'
        return $ LeftMove
    parseRight  = do 
        parseChar '>'
        return $ RightMove

-- Reads the test input
readInputs :: IO [Move]
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseMoves contents)

-- Drops the rock into the tunnel, returning the new tunnel and the new step counter)
dropRock :: [Move] -> Shape -> Tunnel -> Int -> (Tunnel, Int)
dropRock ms s t n0 = dropRock' (x0, y0) n0 where
    (x0, y0) = (2 , (localHeight t) + (length s) + 3)
    dropRock' (x, y) n | jetTurn   = applyJetMove
                       | otherwise  = applyGravity
                       where
                            applyGravity = case checkCollision s x (y - 1) t of
                                True        -> stop
                                False       -> dropRock' (x, y - 1) (n + 1)
                            applyJetMove = case jetMove of  
                                LeftMove    -> case checkCollision s (x - 1) y t of
                                                True    -> dropRock' (x, y)     (n + 1)
                                                False   -> dropRock' (x - 1, y) (n + 1)
                                RightMove   -> case checkCollision s (x + 1) y t of
                                                True    -> dropRock' (x, y)     (n + 1)
                                                False   -> dropRock' (x + 1, y) (n + 1)
                            stop         = (t', n + 1)
                            t'           = trimTunnel $ t { rows = (pasteShape s x y (rows t)), localHeight = (max (localHeight t) y), noofRocks = ((noofRocks t) + 1) }
                            jetTurn      = n `mod` 2 == 0
                            jetMove      = ms !! ((n `div` 2) `mod` (length ms))

-- Drops a number of rocks
dropRocks :: [Move] -> [Shape] -> Int -> (Int -> Int -> Tunnel -> [(Int, Int, Int)] -> Maybe b) -> Tunnel -> (Tunnel, Int, b)
dropRocks ms ss n0 p t = dropRocks' 0 n0 t [] where
    dropRocks' c n t ns  = case p c n t ns of 
                            Just a  -> (t, n, a)
                            Nothing -> dropRocks' (c + 1) n' t' ((c, n, maxHeight t):ns)
                        where 
                            (t', n') = dropRock ms s t n
                            s        = (ss !! (c `mod` (length rocks)))

-- Draws the tunnel
drawTunnel :: Tunnel -> IO ()
drawTunnel t = mapM_ drawRows (reverse [0..maxY]) where
    maxY = localHeight t
    drawRows y | (y + (cachedHeight t)) == (0)          = putStrLn $ "+" ++ ((rows t) !! y) ++ "+"
               | (y + (cachedHeight t)) `mod` 10  == 0  = putStrLn $ "|" ++ ((rows t) !! y) ++ "|- " ++ show (y + (cachedHeight t))
               | otherwise                              = putStrLn $ "|" ++ ((rows t) !! y) ++ "|"

-- A predicate which checks for the number of spawned rocks
untilDropCount :: Int -> Int -> Int -> Tunnel -> [(Int, Int, Int)] -> Maybe Bool
untilDropCount cM c _ _ _ | c == cM     = Just True
                          | otherwise   = Nothing

-- Keeps going until a repeating cycle is detected - returns the (base height, base count, height delta, count delta)
untilRepeatingCycleFound :: [Move] -> Int -> Int -> Int -> Tunnel -> [(Int, Int, Int)] ->  Maybe (Int, Int, Int, Int)
untilRepeatingCycleFound ms w c n t ns  | n > 100000 = error "This does not seem to be working" 
                                        | ((maxHeight t) > w) = runTest
                                        | otherwise           = Nothing
                                        where 
                                            runTest     = case findIndex ((==) ref) tests of
                                                Nothing      -> Nothing
                                                Just x       -> if sameMove && sameCmd && sameRock then Just (baseHeight, c, heightDelta, countDelta) else Nothing where
                                                    -- Checks and frequency
                                                    baseHeight      = maxHeight t
                                                    previousHeight  = maxHeight t - x - 1
                                                    heightDelta     = x + 1
                                                    countDelta      = (c - c0)
                                                    sameRock        = (c `mod` (length rocks)) == (c0 `mod` (length rocks))
                                                    sameMove        = ((n `div` 2) `mod` (length ms)) == ((n0 `div` 2) `mod` (length ms))
                                                    sameCmd         = (n `mod` 2) == (n0 `mod` 2) 
                                                    (c0, n0, h0)    = fromJust $ find (\(_, _, h) -> h == previousHeight) ns
                                                where
                                                    -- Take the last few rows
                                                    ref     = take w (drop (maxHeight t - w + 1) (rows t))  
                                                    tests   = map (\w' -> take w (drop (maxHeight t - w + 1 - w') (rows t))) [1..(maxHeight t - w)]
 
-- Computes the predicted height, given the (base height, base count, height delta, count delta)
-- Returns the predicted height + any amount that could not be predicted
computePredictedHeight :: (Int, Int, Int, Int) -> Int -> (Int, Int)
computePredictedHeight (bh, bc, dh, dc) n | n <= bc   = (0,  n)
                                          | otherwise = (ph, rc)
                                          where 
                                            rc = (n - bc) `mod` dc
                                            ph = bh + ((n - bc) `div` dc) * dh

-- The solver for part #1 of the puzzle
solvePart1 :: [Move] -> IO Int
solvePart1 ms = do
    let (tunnel, n, _) = dropRocks ms rocks 0 (untilDropCount 2022) mkTunnel
    drawTunnel tunnel
    return $ maxHeight tunnel

-- The solver for part #2 of the puzzle
solvePart2 :: [Move] -> IO Int
solvePart2 ms = do 
    -- Start off by running the simulation until a pattern is detected & run some predictions
    let (t, n, p)           = dropRocks ms rocks 0 (untilRepeatingCycleFound ms 1000) mkTunnel
    let (ph, rc)            = computePredictedHeight p 1000000000000

    -- We may need to compute additional rows to arrive at the final height
    let (t', _, _)  = dropRocks ms rocks n (untilDropCount rc) t
    let fh          = if rc > 0 then ((maxHeight t') - (maxHeight t)) + ph else ph
    return fh

-- The full solver
day17Solver :: IO [Int]
day17Solver = do
    input <- readInputs
    part1 <- solvePart1 input
    part2 <- solvePart2 input
    return [part1, part2]
