module Day11 (day11Solver) where

import Common
import Parser
import Data.Char
import Data.Either
import Data.List

-- The input file path
inputFile :: FilePath
inputFile = "res/day11_input.txt"

-- A monkey
data Monkey = Monkey { index :: Int, items :: [Int], operation :: (Int -> Int), test :: (Int -> Int), divisorVal :: Int, inspectCount :: Int }

-- Make monkey an instance of show for debugging purposes
instance Show Monkey where
    show m = "{Monkey " ++ show (index m) ++ ", " ++ show (items m) ++ ", " ++ show (inspectCount m)++ "}"

-- Parse a single command
parseMonkey :: Parser Monkey
parseMonkey = do
    i   <- pLine parseIndex
    is  <- pLine parseStartingItems
    o   <- pLine parseOperation
    t   <- pLine parseTest
    return $ Monkey { index = i, items = is, operation = o, test = (fst t), divisorVal = (snd t), inspectCount = 0 } where
        parseIndex = do
            parseSpaces
            parseString "Monkey "
            i <- parseInt
            parseString ":"
            return i
        parseStartingItems = do
            parseSpaces
            parseString "Starting items: "
            is <- many parseOneItem 
            return is where
                parseOneItem = do
                    parseSpaces
                    n <- parseInt
                    pMaybe $ parseChar ','
                    return n
        parseOperation = do
            parseSpaces
            parseString "Operation: new = old"
            parseSpaces
            op <- parseIsOneOf "*+"
            parseSpaces
            t <- pEither parseInt (parseString "old")
            case op of
                '*'         -> return (\x -> x * (fromLeft x t))
                '+'         -> return (\x -> x + (fromLeft x t))
                otherwise   -> error "Operator not recognised"
        parseTest = do
            parseSpaces
            parseString "Test: divisible by "
            d <- parseInt
            parseSpaces
            parseString "If true: throw to monkey "
            m0 <- parseInt
            parseSpaces
            parseString "If false: throw to monkey "
            m1 <- parseInt
            return ((\x -> if x `mod` d == 0 then m0 else m1), d)

-- Parse commands
parseMonkeys :: Parser [Monkey]
parseMonkeys = many $ parseMonkey

-- Reads the test input
readInputs :: IO [Monkey]
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseMonkeys contents)

-- Computes the items thrown by a monkey
monkeyThrownItems :: (Int -> Int) -> Monkey -> [(Int,Int)]
monkeyThrownItems f m = map (\i -> ((test m) i, i)) is where
    is = map (f . (operation m)) (items m)

-- Gives the monkeys the items
giveMonkeyItems :: [Int] -> Monkey -> Monkey
giveMonkeyItems is m = m { items = ((items m) ++ is) }

-- Executes a single round
executeRound :: (Int -> Int) -> [Monkey] -> [Monkey]
executeRound f ms = map (giveMonkeyItems' (snd mss')) (fst mss') where
    mss'                        = (executeRound' (ms, []))
    executeRound' ([], is)      = ([], is)
    executeRound' (m:ms, is)    = (m':ms', is') where
        m'          = m { items = [], inspectCount = (inspectCount m + (length (items m))) }
        (ms', is')  = executeRound' ((map (giveMonkeyItems' isL) ms), isR)
        isR         = filter (\(x, _) -> not (x `elem` (map index ms))) isM
        isL         = filter (\(x, _) ->     (x `elem` (map index ms))) isM
        isM         = is ++ (monkeyThrownItems f m)
    giveMonkeyItems' is m = giveMonkeyItems is' m where
        is' = map snd (filter (\(x, _) -> x == (index m)) is)

-- Executes a number of rounds & tracks the number of items inspected
executeRounds :: (Int -> Int) -> Int -> [Monkey] -> [Monkey]
executeRounds f n ms = executeRounds' n ms where
    executeRounds' 0 ms = ms
    executeRounds' n ms = executeRounds' n' ms' where
        ms' = executeRound f ms
        n'  = n - 1

-- The solver for part #1 of the puzzle
solvePart1 :: [Monkey] -> Int
solvePart1 ms = (xs !! 0) * (xs !! 1) where
    xs = reverse . sort $ map inspectCount (executeRounds decreaseWorry numberOfRounds ms)
    decreaseWorry   = (\x -> x `div` 3) 
    numberOfRounds  = 20

-- The solver for part #2 of the puzzle
solvePart2 :: [Monkey] -> Int
solvePart2 ms = (xs !! 0) * (xs !! 1) where
    xs = reverse . sort $ map inspectCount (executeRounds decreaseWorry numberOfRounds ms)
    decreaseWorry   = (\x -> x `mod` superModulo)
    superModulo     = product $ map divisorVal ms
    numberOfRounds  = 10000

-- The full solver
day11Solver :: IO [Int]
day11Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
