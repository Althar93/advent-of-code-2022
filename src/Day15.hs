module Day15 (day15Solver) where

import Common
import Parser
import Data.Maybe
import Data.List

-- The input file path
inputFile :: FilePath
inputFile = "res/day15_input.txt"

-- A 2D vector
type Vector2 = (Int, Int)

-- A sensor consisting of a position and range
type Sensor = (Int, Int, Int)

-- A beacon
type Beacon = (Int, Int)

-- A cave
data Cave = Cave { sensors :: ![Sensor], beacons :: ![Beacon] }

-- Parses a sensor
parseSensorAndBeacon :: Parser (Sensor, Beacon)
parseSensorAndBeacon = do
    parseSpaces
    parseString "Sensor at x="
    x <- parseInt
    parseString ", y="
    y <- parseInt
    parseString ": closest beacon is at x="
    xB <- parseInt
    parseString ", y="
    yB <- parseInt
    return ((x, y, range (x, y) (xB, yB)), (xB, yB))

-- Parses many sensors
parseSensorsAndBeacons :: Parser [(Sensor, Beacon)]
parseSensorsAndBeacons = some $ parseSensorAndBeacon

-- Parses a cave
parseCave :: Parser Cave
parseCave = do
    sb <- parseSensorsAndBeacons
    let bs  = removeDuplicates $ map snd sb
    let ss  = map fst sb
    return Cave { sensors = ss, beacons = bs }

-- Reads the test input
readInputs :: IO Cave
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseCave contents)

-- Computes and returns the bounds of the cave (used for drawing)
caveBounds :: Cave -> (Vector2, Vector2)
caveBounds c = (bm, bM) where
    bm = foldl (\(xm, ym) (x, y) -> (min xm x, min ym y) ) a xs
    bM = foldl (\(xM, yM) (x, y) -> (max xM x, max yM y) ) a xs
    a  = (last ss)
    xs = (beacons c)++(init ss)
    ss = map (\(x, y, r) -> (x, y)) (sensors c)

-- Draws the map
drawCave :: Cave -> Int -> IO ()
drawCave c n = mapM_ drawRows [minY..maxY] where
    ((minX, minY), (maxX, maxY))                        = (caveBounds c)
    drawRows y                                          = putStrLn $ map (drawCell y) [minX..maxX]
    drawCell y x | (x, y) `elem` (map (\(x, y, r) -> (x, y)) (sensors c))   = 'S'
                 | (x, y) `elem` (beacons c)                                = 'B'
                 | (x, y) `inRangeOf` (sensors c !! n)                      = '#'
                 | otherwise                                                = '.'

-- Returns whether the position is in range of the sensor
inRangeOf :: Vector2 -> Sensor -> Bool
inRangeOf p (x, y, r) = range p (x, y) <= r

-- Computes the range between two 2D points
range :: Vector2 -> Vector2 -> Int
range (xA, yA) (xB, yB) = range' xA xB + range' yA yB

-- Computes the range between two 1D points
range' :: Int -> Int -> Int
range' xA xB = abs (xA - xB)

-- Returns the sensor bounds on the Y axis
sensorBoundsY :: Sensor -> (Int, Int)
sensorBoundsY (x, y, r) = (y - r, y + r)

-- Returns the sensor bounds on the X axis
sensorBoundsX :: Sensor -> (Int, Int)
sensorBoundsX (x, y, r) = (x - r, x + r)

-- Returns whether the value is between two min/max ordered values
isBetween :: (Ord a) => a -> (a, a) -> Bool
isBetween x (xm, xM) = x >= xm && x <= xM

-- Returns whether the sensor intersects the given row
intersectsRow :: Int -> Sensor -> Bool
intersectsRow y s = y `isBetween` (sensorBoundsY s)

-- Returns the sensor's span for the given row
spanRow :: Int -> Sensor -> (Int, Int)
spanRow y (xs, ys, rs) = (xs - dx, xs + dx) where
    dx = rs - abs(y - ys)  

-- Reduces a set of spans to unique (merged) spans
reduceSpan :: [(Int, Int)] -> [(Int, Int)]
reduceSpan []  = []
reduceSpan [x] = [x]
reduceSpan ((xm0,  xM0):(xm1, xM1):xs)  | xm0 <= xm1 && xM0 >= xM1 = reduceSpan ((xm0, xM0):xs)
                                        | xm0 >= xm1 && xM0 <= xM1 = reduceSpan ((xm1, xM1):xs)
                                        | xm0 <= xM1 && xM0 >= xM1 = reduceSpan ((xm1, xM0):xs)
                                        | xm0 <= xm1 && xM0 >= xm1 = reduceSpan ((xm0, xM1):xs)
                                        | otherwise                = (xm0, xM0):(reduceSpan ((xm1, xM1):xs))

-- Flattens a span into its discrete elements
flattenSpan :: [(Int, Int)] -> [Int]
flattenSpan []              = []
flattenSpan ((xm, xM):xs)   = [xm..xM] ++ (flattenSpan xs)

-- Subtracts the spans from the input one
subtractSpans :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
subtractSpans (xm, xM) [] = [(xm, xM)]
subtractSpans (xm0, xM0) ((xm1, xM1):xs)    | xm0 <= xm1 && xM0 >= xM1  = (subtractSpans (xm0, xm1 - 1) xs) ++ (subtractSpans (xM1 + 1, xM0) xs)
                                            | xm0 >= xm1 && xM0 <= xM1  = []
                                            | xm0 <= xM1 && xM0 >= xM1  = (subtractSpans (xM1 + 1, xM0) xs)
                                            | xm0 <= xm1 && xM0 >= xm1  = (subtractSpans (xm0, xm1 - 1) xs)
                                            | otherwise                 = (subtractSpans (xm0, xM0)  xs)

-- Returns the scanned positions which are empty along the specified row (i.e. cannot contain a beacon)
-- The boolean indicates whether beacons should be included or not from the scan
computeScannedRows :: Int -> Bool -> Cave -> [Vector2]
computeScannedRows y i c = if i then filter (\y -> not (y `elem` (beacons c))) ys else ys where
    ys  = zip (flattenSpan xs) (repeat y)
    xs  = reduceSpan . sort $ map (spanRow y) ss
    ss  = filter (intersectsRow y) (sensors c)

--Computes the shadowed rows (i.e. not covered by any sensor)
computeShadowedRows :: (Int, Int) -> Int -> Cave -> [Vector2]
computeShadowedRows s y c = ys where
    ys = zip (flattenSpan ts) (repeat y) 
    ts = subtractSpans s xs
    xs = reduceSpan . sort $ map (spanRow y) ss
    ss = filter (intersectsRow y) (sensors c)

-- Finds the distress beacon by searching through the specified area
findDistressBeaconInsideArea :: Int -> Cave -> Maybe Vector2
findDistressBeaconInsideArea a c = case filter unitLength es  of
        []          -> Nothing
        [[b]]       -> Just b
        otherwise   -> error $ "More than one solution : " ++ (show es) 
        where
            es              = [(computeShadowedRows (0, a) y c) | y <- [0..a]]
            unitLength [x]  = True
            unitLength  _   = False

-- Computes the tuning frequency based on the beacon's position
tuneFrequency :: Vector2 -> Int
tuneFrequency (x, y) = x * 4000000 + y

-- The solver for part #1 of the puzzle
solvePart1 :: Cave -> Int
solvePart1 c = length $ computeScannedRows y True c where
    y = 2000000 -- 10 for the test input / 2000000 for the real input

-- The solver for part #2 of the puzzle
solvePart2 :: Cave -> Int
solvePart2 c = case findDistressBeaconInsideArea r c of 
    Nothing -> error "No solution found"
    Just b  -> tuneFrequency b 
    where
        r = 4000000 -- 20 for the test input / 4000000 for the real input

-- The full solver
day15Solver :: IO [Int]
day15Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
