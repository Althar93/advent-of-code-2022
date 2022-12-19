module Day16 (day16Solver) where

import Common
import Parser
import Data.Char
import Data.Maybe
import Data.List

-- A valve
data Valve = Valve { 
    name        :: !String,
    rate        :: !Int,
    tunnels     :: ![String],
    open        :: !Bool,
    pressure    :: !Int
} deriving (Eq, Show)

-- The input file path
inputFile :: FilePath
inputFile = "res/day16_test_input.txt"

-- Parses a single valve
parseValve :: Parser Valve
parseValve = do
    parseSpaces
    parseString "Valve "
    n <- parseName
    parseString " has flow rate="
    r <- parseInt
    parseString "; tunnels lead to valves " <|> parseString "; tunnel leads to valve "
    ts <- some $ (parseMoreNames <|> parseName)
    return $ Valve { name = n, rate = r, tunnels = ts, open = False, pressure = 0 }
    where
        parseName       = some $ parseIs isAlphaNum
        parseMoreNames  = do
            t <- parseName
            parseString ", "
            return $ t

-- Parses multiple valves
parseValves :: Parser [Valve]
parseValves = some $ parseValve

-- Reads the test input
readInputs :: IO [Valve]
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseValves contents)

-- Computes the total pressure
--totalPressure :: [Valve] -> Int
--totalPressure vs = foldr ((+) . pressure) 0 vs where

-- Steps the valves once
--stepValves :: [Valve] -> [Valve]
--stepValves []       = []
--stepValves (v:vs)   | open v    = v':(stepValves vs)
--                    | otherwise = v :(stepValves vs)
--                    where 
--                        v' = v { pressure = ((pressure v) + (rate v)) }

-- Simulates the valves for a number of steps (minutes) & returns all possible permutations
--simulateValves :: Int -> [Valve] -> [[Valve]]
--simulateValves n vs = simulateValves' n (name . head vs) vs where
--    simulateValves' 0 _ vs = vs
--    simulateValves' n c vs = 
--
--    ps = flattenPaths vs

-- Finds the valve with the given name
findValve :: [Valve] -> String -> Valve
findValve vs n = case find (((==) n) . name) vs of
    Nothing -> error $ "Couldn't find valve with name " ++ show n
    Just v  -> v

valveIndex :: [Valve] -> String -> Int
valveIndex vs n = case findIndex (((==) n) . name) vs of
    Nothing -> error $ "Couldn't find valve with name " ++ show n
    Just v  -> v

-- Scores a move to a tunnel as the : (timeLeft - timeToMove - timeToOpenValve) * rate
scoreTunnelMove :: Int -> Bool -> (Int, Int) -> Int
scoreTunnelMove t False (r, tm) = (t - tm - 1) * r
scoreTunnelMove t True  (r, tm) = tm

-- Scores the given sequence
scoreSequence :: [Valve] -> [(String, Int)] -> Int
scoreSequence _ []           = 0
scoreSequence vs ((s, t):xs) = ((rate v) * t) + (scoreSequence vs xs) where
    v = findValve vs s

-- Traces & returns the reduced paths for a given valve
tracePaths :: [Valve] -> Valve -> [(String, Int)]
tracePaths vs v = tracePath' [name v] (tunnels v) where
    tracePath' _      []  = []
    tracePath' ss (t:ts)    | t `elem` ss       = (tracePath' (ss) ts)
                            | (rate vt) >  0    = (t, length ss) : (tracePath' ss ts)
                            | otherwise         = (tracePath' (t:ss) (tunnels vt)) ++ (tracePath' ss ts)
                             where
                                vt = findValve vs t

-- Flattens the paths of valves into their unique / reduced paths
flattenPaths :: [Valve] -> [[(String, Int)]]
flattenPaths vs = map (tracePaths vs) vs

-- Finds the path with the corresponding name
findPath :: [(String, Int)] -> String -> (String, Int)
findPath ps n = case find (((==) n) . fst) ps of
    Nothing -> error $ "Couldn't find path for valve " ++ show n
    Just p  -> p

-- Returns the optimal valve opening sequence with timestamps
openValvesSequence :: [Valve] -> String -> Int -> [(String, Int)]
openValvesSequence vs s0 t0 = openValvesSequence' s0 [] t0 where
    ps = flattenPaths vs
    openValvesSequence' s ss t      | t <= 0        = []
                                    | s `elem` ss   = (s, 0) : (openValvesSequence' s' (ss) (t - c))--null vps'    = [(s, t)] --error $ "This should not be happening : " ++ (show s)
                                    | otherwise     = (s, t) : (openValvesSequence' s' (s:ss) (t - c)) where
                                        -- Find the one with the highest score
                                        (s', c)     = findPath vps mn
                                        (mn, _)     = head sst
                                        -- Score each path & sort them
                                        sst         = sortBy (\(_, sc) (_, sc') -> sc `compare` sc') vst
                                        vst         = map (\(n, r, c) -> (n, scoreTunnelMove t (n `elem` ss) (r, c))) vps'
                                        vps'        = map (\(n, c) -> (n, rate (findValve vs n), c)) vps
                                        -- Fetch paths for the current valve
                                        vps         = (ps !! idx)
                                        idx         = valveIndex vs s

-- Returns ALL possible valve traversal sequences, traversing each (working) valve at least once
traverseValvesSequences :: [Valve] -> Int -> String -> [[(String, Int)]]
traverseValvesSequences vs t0 s0 = traverseValvesSequences' ps0 t0 [(s0, t0)] where
    ps0 = flattenPaths vs
    ns  = map name $ filter (\v -> (rate v) > 0) vs
    traverseValvesSequences' :: [[(String, Int)]] -> Int -> [(String, Int)] -> [[(String, Int)]]
    traverseValvesSequences' _  _ []    = []
    traverseValvesSequences' ps t rs    | t <= 0                        = [rs]  
                                        | null p                        = [rs]   
                                        | otherwise                     = left ++ right where
                                            left                        = traverseValveSingle ps' t (s1, c1) rs 
                                            right                       = concatMap (\(s, c) -> traverseValveSingle ps' t (s, c) rs) p'
                                            ps'                         = lps++(p':rps)
                                            ((s1, c1):p')               = p
                                            (lps, p:rps)                = splitAt idx ps
                                            idx                         = valveIndex vs s0
                                            (s0, _)                     = head rs
    traverseValveSingle ps t (s, c) rs  | s `elem` (map fst rs) = traverseValvesSequences' ps t'      ((s,  0     ):rs) 
                                        | otherwise             = traverseValvesSequences' ps (t'- 1) ((s,  t' - 1):rs)  
                                        where
                                            t' = t - c 

-- Seems to 'work' but oh sooo slooow.....
findOptimalSequence :: [Valve] -> Int -> String -> Int
findOptimalSequence vs t0 s0 = findOptimalSequence' vs t0 s0 [] 0 where
    ps = flattenPaths vs
    ns = map name $ filter (\v -> (rate v) > 0) vs
    findOptimalSequence' vs t s os n    | t <= 0                                    = 0
                                        | all (\n -> n `elem` os) ns                = 0
                                        | (length os) >= 3 && (s == head (tail os)) = 0
                                        | s `elem` os                               = moveTo 
                                        | otherwise                                 = max openValve moveTo 
                                        where
                                            openValve = ((t - 1) * (rate v)) + (findOptimalSequence' vs (t - 1) s (s:os) (n+1))
                                            --moveTo    = maximum $ map (\(s', t') -> findOptimalSequence' vs (t - t') s' os (n+1)) p
                                            --moveTo    = foldl (\a (s', t') -> max a (findOptimalSequence' vs (t - t') s' os (n+1))) 0 p
                                            moveTo    = foldr (\(s', t') a -> max a (findOptimalSequence' vs (t - t') s' os (n+1))) 0 p
                                            p         = ps !! (valveIndex vs s)
                                            v         = findValve vs s


-- The solver for part #1 of the puzzle
solvePart1 :: [Valve] -> Int
solvePart1 vs = 0--(scoreSequence vs) $ openValvesSequence vs "AA" 30

-- The solver for part #2 of the puzzle
solvePart2 :: [Valve] -> Int
solvePart2 vs = 0

drawArray :: (Show a) => [a] -> IO ()
drawArray []     = return ()
drawArray (x:xs) = do 
    putStrLn $ show x
    drawArray xs

-- The full solver
day16Solver :: IO [Int]
day16Solver = do
    input <- readInputs
    --putStrLn $ show input
    --putStrLn $ show (totalPressure input)
    --let valve = findValve input "DD" 
    --let trace = tracePaths input valve
    --putStrLn $ show trace

    --putStrLn $ "Sequence"
    --let sequ  = openValvesSequence input "AA" 30
    --putStrLn $ show sequ

    --putStrLn $ "Traversal"
    --let trav  = traverseValvesSequences input 30 "AA"
    --let trav' = map (filter (\(_, n) -> n > 0)) trav
    --putStrLn $ show trav'
    --drawArray trav'
    --mapM_ $ (\x -> putStrLn $ show x) trav'

    putStrLn $ "DFS"
    let optimal = findOptimalSequence input 1 "AA"
    putStrLn $ show optimal

    --putStrLn $ "Score"
    --let scores = map (scoreSequence input) trav 
    --putStrLn $ show scores
    --let maxScore = maximum scores
    --putStrLn $ "Best calculated score : " ++ show maxScore

    -- For testing / comparison
    --let bestSequence = [("DD", 28), ("BB", 25), ("JJ", 21), ("HH", 13), ("EE", 9), ("CC", 6)]
    --let bestScore    = (scoreSequence input) bestSequence
    --putStrLn $ "Best score (test) : " ++ show bestScore

    --putStrLn $ "Test"
    --let test = map name $ filter (\v -> (rate v) > 0) input--filter (((>) 0) . rate) input
    --putStrLn $ show test
    --let score  = scoreSequence input tests
    --putStrLn $ show testaa
    --putStrLn $ show testbb
    --putStrLn $ show testhh
    --putStrLn $ show sequ
    --putStrLn $ show score
    return [solvePart1 input, solvePart2 input]
