module Day19 (day19Solver) where

import Common
import Parser
import Data.List
import Data.Maybe

-- The input file path
inputFile :: FilePath
inputFile = "res/day19_test_input.txt"

-- The different resource types
data Resource = Ore | Clay | Obsidian | Geode deriving (Show, Eq, Enum)

-- The different robot types
data RobotFactory = RobotFactory { robots :: [Int], resources :: [Int] } deriving (Show)

-- The blueprint type
data Blueprint = Blueprint { index :: Int, requirements :: [[Int]] } deriving (Show)

-- A construction function
type ConstructionFunction = Blueprint -> [Int] -> [Int] -> ([Int], [Int])

-- Parses a single blueprint
parseBlueprint :: Parser Blueprint
parseBlueprint = do
    parseSpaces
    parseString "Blueprint "
    i <- parseInt
    parseString ": Each ore robot costs "
    orc <- parseInt
    parseString " ore. Each clay robot costs "
    crc <- parseInt
    parseString " ore. Each obsidian robot costs "
    orc0 <- parseInt
    parseString " ore and "
    orc1 <- parseInt
    parseString " clay. Each geode robot costs "
    grc0 <- parseInt
    parseString " ore and "
    grc1 <- parseInt
    parseString " obsidian."
    return $ Blueprint { index = i, requirements = [[orc, 0, 0, 0], [crc, 0, 0, 0], [orc0, orc1, 0, 0], [grc0, 0, grc1, 0]] }

-- Parses many blueprints
parseBlueprints :: Parser [Blueprint]
parseBlueprints = some $ parseBlueprint

-- Reads the test input
readInputs :: IO [Blueprint]
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseBlueprints contents)

-- Makes a robot factory with one ore-collecting robot
mkRobotFactory :: RobotFactory
mkRobotFactory = RobotFactory { robots = [1, 0, 0, 0], resources = [0, 0, 0, 0] }

-- Returns the ore stored in a robot factory
ore :: RobotFactory -> Int
ore rf = (resources rf) !! (fromEnum Ore)

-- Returns the clay stored in a robot factory
clay :: RobotFactory -> Int
clay rf = (resources rf) !! (fromEnum Clay)

-- Returns the obsidian stored in a robot factory
obsidian :: RobotFactory -> Int
obsidian rf = (resources rf) !! (fromEnum Obsidian)

-- Returns the geodes stored in a robot factory
geodes :: RobotFactory -> Int
geodes rf = (resources rf) !! (fromEnum Geode) 

-- Builds a resource mask
resourceMask :: Resource -> [Int]
resourceMask Ore        = [1, 0, 0, 0]
resourceMask Clay       = [0, 1, 0, 0]
resourceMask Obsidian   = [0, 0, 1, 0]
resourceMask Geode      = [0, 0, 0, 1]

-- Returns a just if the specified robot can be built with the resources consumed, or nothing
canBuildRobot :: Blueprint -> [Int] -> Resource -> Maybe (Resource, [Int])
canBuildRobot bp rcont0 res = case all ((<=) 0) rcont1 of 
    True  -> Just (res, rrequi)
    False -> Nothing
    where
        rcont1 = zipWith (-) rcont0 rrequi
        rrequi = (requirements bp) !! (fromEnum res) 

shouldBuildRobot :: Blueprint -> [Int] -> Resource -> Bool
shouldBuildRobot bp robts0 Ore      = True --(robts0 !! (fromEnum Ore))         < 4 
shouldBuildRobot bp robts0 Clay     = True --(robts0 !! (fromEnum Clay))        < 14
shouldBuildRobot bp robts0 Obsidian = True --(robts0 !! (fromEnum Obsidian))    < 7
shouldBuildRobot bp robts0 Geode    = True

-- Uses the blueprint to construct robots & returns a tuple containing the number of robots constructed & resource cost
constructRobotsHeuristics :: Blueprint -> [Int] -> [Int] -> ([Int], [Int])
constructRobotsHeuristics bp rcont0 robts0 = constructRobots' sbuil where
    sbuil = zipWith (\a b -> if a then b else Nothing) srbts mrbts
    srbts = map (shouldBuildThisRobot)          [Geode, Obsidian, Clay, Ore]
    mrbts = map (canBuildRobot bp rcont0)       [Geode, Obsidian, Clay, Ore]
    constructRobots' []     = ([0, 0, 0, 0], [0, 0, 0, 0])
    constructRobots' (x:xs) = case x of
        Just (r, rconsu)    -> (resourceMask r, rconsu)
        Nothing             -> constructRobots' xs
    shouldBuildThisRobot Ore      = robts0 !! (fromEnum Ore)      <= 4--((requirements bp) !! (fromEnum Ore))
    shouldBuildThisRobot Clay     = robts0 !! (fromEnum Clay)     <= 14--((requirements bp) !! (fromEnum Clay))
    shouldBuildThisRobot Obsidian = robts0 !! (fromEnum Obsidian) <= 7--((requirements bp) !! (fromEnum Obsidian))
    shouldBuildThisRobot Geode    = True --robts0 !! (fromEnum Clay) <= 4

-- Construct robots based on a pre-defined sequence
constructRobotsFromSequence :: [Resource] -> Blueprint -> [Int] -> [Int] -> ([Int], [Int])
constructRobotsFromSequence xs bp rcont0 robts0 = constructRobots' sbuil where
    sbuil = zipWith (\a b -> if a then b else Nothing) srbts mrbts
    srbts = map (shouldBuildThisRobot)          [Geode, Obsidian, Clay, Ore]
    mrbts = map (canBuildRobot bp rcont0)       [Geode, Obsidian, Clay, Ore]
    constructRobots' []     = ([0, 0, 0, 0], [0, 0, 0, 0])
    constructRobots' (x:xs) = case x of
        Just (r, rconsu)    -> (resourceMask r, rconsu)
        Nothing             -> constructRobots' xs
    shouldBuildThisRobot x  = (idx < length xs) && x == (xs !! idx) where
        idx = (sum robts0 - 1)

-- Steps the robot factory one step
stepRobotFactorySingle :: Blueprint -> ConstructionFunction -> RobotFactory -> RobotFactory
stepRobotFactorySingle bp cf rf = rf { resources = rcont1, robots = robts1 } where
    -- Resource gathering/spending
    rcont1              = zipWith (+) rcont0 rdelta
    rdelta              = zipWith (-) rcolct rconsu
    rcolct              = robts0
    rcont0              = (resources rf)
    -- Robot contruction
    robts1              = zipWith (+) robtsc robts0
    (robtsc, rconsu)    = cf bp rcont0 robts0
    robts0              = (robots rf)

-- Steps the robot factory a number of steps
stepRobotFactory :: Int -> Blueprint -> ConstructionFunction -> RobotFactory -> RobotFactory
stepRobotFactory 0 _  cf rf = rf 
stepRobotFactory t bp cf rf = stepRobotFactory (t - 1) bp cf rf' where
    rf' = stepRobotFactorySingle bp cf rf

-- Constructs all possible sequences of robots (with some minor pruning)
constructRobotSequences :: Blueprint -> Int -> [[Resource]]
constructRobotSequences bp nM = map reverse $ filter (validSequence) (constructRobotSequences' 0 []) where
    constructRobotSequences' :: Int -> [Resource] -> [[Resource]]
    constructRobotSequences' n xs | n == nM                                 = [xs]
                                  | n == nM - 1                             = constructRobotSequences' (n+1) (Geode:xs)
                                  | n == nM - 2                             = constructRobotSequences' (n+1) (Ore:xs) ++ constructRobotSequences' (n+1) (Obsidian:xs) ++ constructRobotSequences' (n+1) (Geode:xs)
                                  | Clay `elem` xs && Obsidian `elem` xs    = constructRobotSequences' (n+1) (Ore:xs) ++ constructRobotSequences' (n+1) (Clay:xs) ++ constructRobotSequences' (n+1) (Obsidian:xs) ++ constructRobotSequences' (n+1) (Geode:xs) 
                                  | Clay `elem` xs                          = constructRobotSequences' (n+1) (Ore:xs) ++ constructRobotSequences' (n+1) (Clay:xs) ++ constructRobotSequences' (n+1) (Obsidian:xs) 
                                  | otherwise                               = constructRobotSequences' (n+1) (Ore:xs) ++ constructRobotSequences' (n+1) (Clay:xs)
    -- Try and filter out any sequence that makes no sense, such as those that do not produce any geodes
    validSequence xs = (Geode `elem` xs) && (Obsidian `elem` xs) && (Clay `elem` xs)
    count x xs       = (length (filter (==x) xs))

-- Steps the robot factory in a DFS manner, returning the best performing one
stepRobotFactoryDFS :: Int -> Blueprint -> RobotFactory -> [RobotFactory]
stepRobotFactoryDFS tM bp rf0 = (stepRobotFactoryDFS' 0 rf0) -- selectRobotFactory $ (stepRobotFactoryDFS' 0 rf0)
    where
        -- Keep stepping through factories
        stepRobotFactoryDFS' t rf   | t == tM       = [rf]--if (geodes rf) > 0 then [rf] else [rf]s
                                    | t == 2        = error $ "Build list is " ++ show finalBuildList ++ " for " ++ show rf--
                                    | otherwise     = concatMap (\mx -> stepRobotFactoryDFS' (t+1) (constructFactory mx rf)) finalBuildList
                                    where
                                        finalBuildList  = shouldBuildList ++ [Nothing]
                                        shouldBuildList = filter (shouldBuild rf) (canBuild rf)
        -- Construct a new factory
        constructFactory mx rf      = stepRobotFactorySingle bp (constructRobotsFromPriority mx) rf
        -- What can we build this time round?
        canBuild rf                 = mapJusts fst (map (canBuildRobot bp (resources rf)) [Geode, Obsidian, Clay, Ore])
        -- Should we build it
        shouldBuild rf Nothing      = False
        shouldBuild rf (Just r)     = shouldBuildRobot bp (robots rf) r

-- Constructs robots based on a priority list
constructRobotsFromPriority :: Maybe Resource -> Blueprint -> [Int] -> [Int] -> ([Int], [Int])
constructRobotsFromPriority mx bp rcont0 robts0 = case mx of
    Nothing  -> ([0, 0, 0, 0], [0, 0, 0, 0])
    Just x   -> constructRobotsFromSequence [x] bp rcont0 robts0

-- Returns the best robot factory out of a list of robot factories
selectRobotFactory :: [RobotFactory] -> RobotFactory
selectRobotFactory [] = error $ "No robots factories provided"
selectRobotFactory xs = head $ sortBy (\a b -> (geodes a) `compare` (geodes b)) xs

-- Computes the quality level of a blueprint
computeBlueprintQualityLevel :: Blueprint -> Int -> Int
computeBlueprintQualityLevel bp n = (index bp) * n

-- The solver for part #1 of the puzzle
solvePart1 :: [Blueprint] -> [[Resource]] -> Int
solvePart1 bs pss = maximum gs where
    gs  = map (\rf -> last (resources rf)) rfs
    rfs = map (\ps -> stepRobotFactory 24 bp (constructRobotsFromSequence ps) rf) pss 
    rf  = mkRobotFactory
    bp  = (bs !! 0)

-- The solver for part #2 of the puzzle
solvePart2 :: [Blueprint] -> [[Resource]] -> Int
solvePart2 bs pss = 0

-- The full solver
day19Solver :: IO [Int]
day19Solver = do
    input <- readInputs

    -- Build all possible (sensible) permutations 
    putStrLn $ "Generating permutations..."
    let permutations = constructRobotSequences (input !! 0) 5--15 --13 --14
    putStrLn $ "Done generating " ++ ((show . length) permutations) ++ " permutations..."
    --putStrLn $ show permutations

    -- Test DFS
    let bp  = input !! 0
    let rf  = mkRobotFactory
    let rf' = stepRobotFactoryDFS 3 bp rf
    let rfM = selectRobotFactory rf'
    mapM_ (putStrLn . show) rf'
    putStrLn $ (show (length rf')) ++ " Robot factories returned..."
    putStrLn $ show "Factory produced " ++ (show (geodes rfM)) ++ " geode(s)."

    -- Test input
    --let bp  = input !! 1
    --let rf  = mkRobotFactory
    --let cf  = constructRobotsFromSequence [Clay, Clay, Clay, Obsidian, Clay, Obsidian, Geode, Geode]
    --let cf  = constructRobotsFromSequence [Ore, Ore, Clay, Clay, Clay, Clay, Clay, Obsidian, Obsidian, Obsidian, Clay, Obsidian, Obsidian, Geode, Obsidian, Geode, Geode]
    --let rf' = stepRobotFactory 24 bp cf rf
    --putStrLn $ show rf' 
    --putStrLn $ "Factory produced " ++ (show (last (resources rf'))) ++ " geode(s)."


    return [solvePart1 input permutations, solvePart2 input permutations]
