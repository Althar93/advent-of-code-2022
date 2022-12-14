module Day14 (day14Solver) where

import Common
import Parser
import Data.Maybe

-- A vector 2
type Vector2 = (Int, Int)

--A wall defined as a set of corners
type WallSection = [Vector2]

-- A cave
data Cave = Cave { 
    sandPourPos :: Vector2, 
    hasFloor    :: Bool, 
    limitY      :: Int, 
    wallCells   :: [Vector2], 
    sandCells   :: [Vector2]
}

-- The input file path
inputFile :: FilePath
inputFile = "res/day14_input.txt"

-- Parses a single wall section
parseWallSection :: Parser WallSection
parseWallSection = do
    parseSpaces
    ws <- some $ parseMore <|> parseOne 
    return ws 
    where
    parseMore = do
        p <- parseOne
        parseString " -> "
        return p
    parseOne = do
        x <- parseInt
        parseChar ','
        y <- parseInt
        return (x, y)

-- Parses wall sections
parseCave :: Parser Cave
parseCave = do
    ws <- many $ parseWallSection

    let s  = (500, 0)
    let w  = (buildWallList ws)
    let ly = foldl1 max (map snd w)

    return Cave { sandPourPos = s, hasFloor = False, limitY = ly, wallCells = w, sandCells = [] }

-- Reads the test input
readInputs :: IO Cave
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseCave contents)

-- Builds the list of unique walls
buildWallList :: [WallSection] -> [Vector2]
buildWallList []        = []
buildWallList (w:ws)    = removeDuplicates $ (buildWallList' w) ++ (buildWallList ws) where
    buildWallList' ((xw0, yw0):(xw1, yw1):ps)   | (xw0 == xw1) = [(xw0, y) | y <- [min yw0 yw1..max yw0 yw1]] ++ (buildWallList' ((xw1, yw1):ps))
                                                | (yw0 == yw1) = [(x, yw0) | x <- [min xw0 xw1..max xw0 xw1]] ++ (buildWallList' ((xw1, yw1):ps))
                                                | otherwise    = error "Diagonal walls not supported"
    buildWallList' _    = []

-- Adds a floor to the cave
mkFloor :: Cave -> Cave
mkFloor c = c { hasFloor = True, limitY = (2 + (limitY c)) } 

-- Adds a resting grain of sand to the cave
addSand :: Vector2 -> Cave -> Cave
addSand s c = c { sandCells = (s:(sandCells c)) }

-- Computes and returns the bounds of the cave (used for drawing)
caveBounds :: Cave -> (Vector2, Vector2)
caveBounds c = (bm, bM) where
    bm = foldl (\(xm, ym) (x, y) -> (min xm x, min ym y) ) a xs
    bM = foldl (\(xM, yM) (x, y) -> (max xM x, max yM y) ) a xs
    a  = (fst (sandPourPos c), (limitY c))
    xs = ((sandPourPos c):(sandCells c)++(wallCells c))

-- Returns whether the specified position is a wall
isWall :: Vector2 -> Cave -> Bool
isWall p c = p `elem` (wallCells c)

-- Returns whether the specified position is sand
isSand :: Vector2 -> Cave -> Bool
isSand p c = p `elem` (sandCells c)

-- Returns whether the specified position is floor
isFloor :: Vector2 -> Cave -> Bool
isFloor p c | hasFloor c = (snd p) == (limitY c)
            | otherwise  = False

--Returns whether the specified position is blocked
isBlocked :: Vector2 -> Cave -> Bool
isBlocked p c = (isWall p c) || (isSand p c)

-- Draws the map
drawCave :: Cave -> IO ()
drawCave c = mapM_ drawRows [minY..maxY] where
    ((minX, minY), (maxX, maxY))                = (caveBounds c)
    drawRows y                                  = putStrLn $ map (drawCell y) [minX..maxX]
    drawCell y x | (x, y) == (sandPourPos c)    = '+'
                 | isWall  (x, y) c             = '#'
                 | isFloor (x, y) c             = '#'
                 | isSand  (x, y) c             = 'o'
                 | otherwise                    = '.'

-- Produces a number of sand units until overflow is reached (i.e. sand starts to fall forever)
produceSandUntilOverflow :: Vector2 -> Cave -> Cave
produceSandUntilOverflow s c = produceSandUntilOverflow' [s] c where
    produceSandUntilOverflow' ps0 c = case produceSandUnit (last ps0) c of
        Nothing  -> c
        Just ps  -> produceSandUntilOverflow' (init ps') (addSand (last ps) c) where
            ps'  = (takeWhile (not . (==) (head ps)) ps0) ++ ps

produceSandUntilClogged :: Vector2 -> Cave -> Cave
produceSandUntilClogged s c = produceSandUntilClogged' [s] c where
    produceSandUntilClogged' ps0 c = case produceSandUnit (last ps0) c of
        Nothing  -> c
        Just ps  -> if (last ps) == (sandPourPos c) then (addSand (last ps) c) else produceSandUntilClogged' (init ps') (addSand (last ps) c) where
            ps'  = (takeWhile (not . (==) (head ps)) ps0) ++ ps

concatMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
concatMaybe Nothing _               = Nothing
concatMaybe _ Nothing               = Nothing
concatMaybe  (Just xs) (Just ys)    = Just (xs++ys)

-- Produces a sand unit & returns its resting position
produceSandUnit :: Vector2 -> Cave -> Maybe [Vector2]
produceSandUnit s c = takeRestingPosition $ (moveSand c s) where     
    -- Resting position or nothing & account for the presence of a floor
    takeRestingPosition []                                                       = Just []                                      
    takeRestingPosition (p@(_, y):ps)   | y >= (limitY c) - 1 && (hasFloor c)    = Just [p]
                                        | y >= (limitY c)                        = Nothing
                                        | otherwise                              = concatMaybe (Just [p]) (takeRestingPosition ps) 
    -- Move the sand grain along
    moveSand c (x, y) | (isBlocked (x, y + 1) c) && (isBlocked (x - 1, y + 1) c) && (isBlocked (x + 1, y + 1) c)    = [(x, y)]
                      | (isBlocked (x, y + 1) c) && not (isBlocked (x - 1, y + 1) c)                                =  (x, y) : (moveSand c (x - 1, y + 1))
                      | (isBlocked (x, y + 1) c) && not (isBlocked (x + 1, y + 1) c)                                =  (x, y) : (moveSand c (x + 1, y + 1))
                      | otherwise                                                                                   =  (x, y) : (moveSand c (x + 0, y + 1))

-- The solver for part #1 of the puzzle
solvePart1 :: Cave -> IO Int
solvePart1 c = do 
    drawCave c' 
    return $ length (sandCells c') where
        c' = produceSandUntilOverflow (sandPourPos c) c

-- The solver for part #2 of the puzzle
solvePart2 :: Cave -> IO Int
solvePart2 c = do
    drawCave c' 
    return $ length (sandCells c') where
        c' = produceSandUntilClogged (sandPourPos c) (mkFloor c)

-- The full solver
day14Solver :: IO [Int]
day14Solver = do
    input <- readInputs
    part1 <- solvePart1 input
    part2 <- solvePart2 input
    return [part1, part2]
