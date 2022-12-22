module Day18 (day18Solver) where

import Common
import Parser
import Data.List
import Data.Maybe

-- The input file path
inputFile :: FilePath
inputFile = "res/day18_input.txt"

--A 3D vector and/cell
type Vector3 = (Int, Int, Int)

-- Parses a single cube
parseCube :: Parser Vector3
parseCube = do
    parseSpaces
    x <- parseInt
    parseChar ','
    y <- parseInt
    parseChar ','
    z <- parseInt
    return (x, y, z)

-- Parses some cubes
parseCubes :: Parser [Vector3]
parseCubes = some $ parseCube

-- Reads the test input
readInputs :: IO [Vector3]
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseCubes contents)

-- Computes the surface of a cell, given a list of neighbours
computeCellSurface :: [Vector3] -> Int
computeCellSurface ns = 6 - length(ns)

-- Returns neighbouring cells
neighbouringCells :: [Vector3] -> Vector3 -> [Vector3]
neighbouringCells [] _     = []
neighbouringCells ((x', y', z'):cs) (x, y, z) | (abs(x - x') + abs(y - y') + abs(z - z')) == 1  = (x', y', z') : (neighbouringCells cs (x, y, z))
                                              | otherwise                                       = neighbouringCells cs (x, y, z)

-- Returns the exposed cells 
exposedDirections :: [Vector3] -> Vector3 -> [Vector3]
exposedDirections ns (x, y, z) = map fst $ filter (\(d, c) -> not (c `elem` ns)) cs where
    cs = map (\(dx, dy, dz) -> ((dx, dy, dz),(x + dx, y + dy, z + dz))) [(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)]

-- Extrudes the cell in the given direction & length
extrudeCell :: [Vector3] -> Int -> Vector3 -> [Vector3]
extrudeCell cs nm c = concatMap (extrudeCell' cs c 1) es where
    ns = neighbouringCells cs c
    es = exposedDirections ns c
    extrudeCell' :: [Vector3] -> Vector3 -> Int -> Vector3 -> [Vector3]
    extrudeCell' cs c n e | n > nm          = []
                          | c' `elem` cs    = []
                          | otherwise       = c':(extrudeCell' cs c (n + 1) e)
                          where
                            c'                            = c `add` (e `mul` n)
                            add (xa, ya, za) (xb, yb, zb) = (xa + xb, ya + yb, za + zb)
                            mul (xa, ya, za) f            = (xa * f , ya * f , za * f )

-- The solver for part #1 of the puzzle
solvePart1 :: [Vector3] -> Int
solvePart1 xs = sum $ map computeCellSurface ns where
    -- Build the list of neighbouring cells for our volume
    ns = map (neighbouringCells xs) xs 

-- The solver for part #2 of the puzzle
solvePart2 :: [Vector3] -> Int
solvePart2 xs = sts - sis where
    -- Compute total & interior surface
    sis     = sum $ map computeCellSurface nis
    sts     = sum $ map computeCellSurface nts
    -- Same as for part 1, except this time we have the inner volume & exterior volumes
    nis     = map (neighbouringCells is') is' 
    nts     = map (neighbouringCells xs ) xs
    -- To disambiguate cells which are inside from those which are merely occluded, only keep the cells which are fully surrounding by internal and/or external cells
    -- TODO : To work in the general case, we would need to invoke this several times - a single iteration will only work for shallow alcoves/bays
    is'     = map fst is
    is      = filter (\(c, ss) -> (computeCellSurface ss) == 0) nos'
    nos'    = zip os' nos 
    nos     = map (neighbouringCells (xs++os')) os' 
    -- Look for any cell which exists exactly 6 times : this means they are either inside OR occluded
    os'     = map head os
    os      = filter (((==) 6) . length) ((group . sort) ess)
    -- Extrude all cells outwards
    ess     = concatMap (extrudeCell xs n) xs 
    -- Arbitrary extrusion amount (should probably be the max extents of the volume to be sure)
    n       = 20

-- The full solver
day18Solver :: IO [Int]
day18Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
