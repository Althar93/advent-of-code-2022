module Day12 (day12Solver) where

import Common
import Parser
import Data.Char
import Data.Maybe
import Data.List

-- The input file path
inputFile :: FilePath
inputFile = "res/day12_input.txt"

-- A position
type Position = (Int, Int)

-- A list of paths
type Path = [Position]

-- The map type
data Map = Map { cells :: [[Int]], start :: Position, end :: Position }

-- Returns the position of the first occurence, if any, of value in list
elemPosition :: (Eq a) => a -> [[a]] -> Maybe Position
elemPosition t xss = elemPosition' t 0 xss where
    elemPosition' _ _ []        = Nothing
    elemPosition' t y (cs:css)  = case elemIndex t cs of
        Just x  -> Just (x, y)
        Nothing -> elemPosition' t (y + 1) css

-- Converts a map character to the corresponding height
charToHeight :: Char -> Int
charToHeight 'S' = 0
charToHeight 'E' = 25
charToHeight  c  = (ord c) - (ord 'a')

-- Converts a map of heights to the corresponding char
heightToChar :: Int -> Char
heightToChar n = chr $ n + (ord 'a')

-- Reads the test input
readInputs :: IO Map
readInputs = do
    contents <- readFile inputFile
    let css     = lines contents
    let css'    = map (map charToHeight) css
    let s       = fromJust (elemPosition 'S' css)
    let e       = fromJust (elemPosition 'E' css)
    return $ Map { cells = css', start = s, end = e}

-- Maps a function to a field
traverseField :: (Int -> Int -> a -> b) -> [[a]] -> [[b]]
traverseField f sss = traverseFieldRow f 0 sss where
    traverseFieldRow _ _ []         = []
    traverseFieldRow f y (ss:sss)   = traverseFieldCell f 0 y ss : traverseFieldRow f (y + 1) (sss) where
        traverseFieldCell _ _ _ []      = []
        traverseFieldCell f x y (s:ss)  = f x y s : traverseFieldCell f (x + 1) y (ss)

-- Short-hand to sample inside a field
at :: [[a]] -> (Int, Int) -> a
at sss (x, y) = (sss !! y) !! x

-- Builds a map of possible moves from any cell
buildMoveMap :: Map -> [[Path]]
buildMoveMap m = traverseField (buildMoves sss) sss where
    sss = (cells m)
    buildMoves sss x y h = filter (\p-> (sss `at` p) <= h + 1) (u++d++l++r) where
        u = if y > 0                        then [(x + 0, y - 1)] else []
        d = if y < (length sss - 1)         then [(x + 0, y + 1)] else []
        l = if x > 0                        then [(x - 1, y + 0)] else []
        r = if x < (length (head sss) - 1)  then [(x + 1, y + 0)] else []

-- Builds a reverse move map
buildMoveMapInverse :: Map -> [[Path]]
buildMoveMapInverse m = traverseField (buildMoves sss) sss where
    sss = (cells m)
    buildMoves sss x y h = filter (\p -> h <= (sss `at` p) + 1 ) (u++d++l++r) where
        u = if y > 0                        then [(x + 0, y - 1)] else []
        d = if y < (length sss - 1)         then [(x + 0, y + 1)] else []
        l = if x > 0                        then [(x - 1, y + 0)] else []
        r = if x < (length (head sss) - 1)  then [(x + 1, y + 0)] else []

-- Builds a map of possible moves from any cell
buildCoordMap :: Map -> [[Position]]
buildCoordMap m = traverseField buildCoords (cells m) where
    buildCoords x y _ = (x, y)

-- Computes all possible paths (breadth-first search)
computePathsBF :: [[Path]] -> Position -> (Position -> Bool) -> [Path]
computePathsBF mm pS fpE = fst $ computePaths' ([],[[pS]]) where
    computePaths' (css, pss) = case pss' of 
        -- No more alternate paths, we are done
        []          -> (css++css', []) 
        -- Still some paths needing to be computed
        otherwise   -> computePaths' (css++css', pss') 
        where
            -- Partition between those paths that have reached the end & those newly generated paths 
            (css', pss')     = partition (\ps -> fpE (head ps)) pps' -- Parition those that reached the end
            -- Generate the list of unique (by their end point) paths which do not exceed the length of a pre-existing successful path
            pps'             = reduceCommonPaths $ generatePaths mm pss 

-- Generates the list of possible paths 
generatePaths :: [[Path]] -> [Path] -> [Path]
generatePaths _  []     = []
generatePaths mm (p:ps) = (generatePaths' mm p) ++ (generatePaths mm ps) where
    generatePaths' mm p = case xs of
        []          -> []
        otherwise   -> zipWith (:) xs (repeat p)
        where
            xs = filter (\x -> not (x `elem` p)) (mm `at` (head p))

-- Reduces the list of paths by removing any redundant entries (end point shared with another pre-existing path)
reduceCommonPaths :: [Path] -> [Path]
reduceCommonPaths ps = reduceCommonPaths' ps [] where
    reduceCommonPaths' [] ps'        = ps'
    reduceCommonPaths' (p:ps) ps'    = case any (elem (head p)) (ps++ps') of
        True  -> reduceCommonPaths' ps ps'
        False -> reduceCommonPaths' ps (p:ps')

-- Returns the shortest path out of a list of paths
shortestPath :: [Path] -> Path
shortestPath pss = head $ sortBy (\a b -> (length a) `compare` (length b)) pss

-- Draws the map
drawMap :: Map -> IO ()
drawMap m = mapM_ (drawRows css) [0..(length css - 1)] where
    css          = cells m
    s            = start m
    e            = end m
    drawRows m y = putStrLn $ map (drawCell css y) [0..(length (head css) - 1)]
    drawCell m y x  | (x, y) == s = 'S'
                    | (x, y) == e = 'E'
                    | otherwise   = heightToChar (css `at` (x, y))

-- The solver for part #1 of the puzzle
solvePart1 :: Map -> Int
solvePart1 m = (length $ shortestPath (computePathsBF mm s e)) - 1 where
        mm = buildMoveMap m
        s  = start m
        e  = (==) (end m)

-- The solver for part #2 of the puzzle
solvePart2 :: Map -> Int
solvePart2 m = (length $ shortestPath (computePathsBF mm s e)) - 1 where
        mm = buildMoveMapInverse m
        s  = (end m)
        e  = (\p -> ((cells m) `at` p) == 0)

-- The full solver
day12Solver :: IO [Int]
day12Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
