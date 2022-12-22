module Day21 (day21Solver) where

import Common
import Parser
import Data.Char
import Data.Maybe
import Data.List

-- A monkey consisting of a tag and operation
type Monkey = (String, Operator)

--An operation
data Operator = Operator Char String String | Const Int | Variable deriving (Show) 

-- The input file path
inputFile :: FilePath
inputFile = "res/day21_input.txt"

-- Parses a tag
parseTag :: Parser String
parseTag = some $ parseIs isAlphaNum

-- Parses a single monkey
parseMonkey :: Parser Monkey
parseMonkey = do
    parseSpaces
    tag <- parseTag
    parseString ": "
    op  <- parseOperation <|> parseConst
    return $ (tag, op)
    where
        parseOperation = do
            a <- parseTag
            parseSpaces
            op <- parseIsOneOf "+-*/"
            parseSpaces
            b <- parseTag
            return $ Operator op a b
        parseConst = do
            n <- parseInt
            return $ Const n

-- Parses several monkeys
parseMonkeys :: Parser [Monkey]
parseMonkeys = some $ parseMonkey

-- Reads the test input
readInputs :: IO [Monkey]
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseMonkeys contents)

-- Replaces the specified monkey with a different once
replaceMonkey :: [Monkey] -> Monkey -> [Monkey]
replaceMonkey []     _            = []
replaceMonkey (x:xs) m@(t, _)   | fst x == t = m:xs
                                | otherwise  = x:(replaceMonkey xs m)

-- Finds the monkey with the specified tag
findMonkey :: [Monkey] -> String -> Maybe Monkey
findMonkey ms t = find ((==t) . fst) ms

-- Resolves the monkey
resolveMonkey :: [Monkey] -> Monkey -> Maybe Int
resolveMonkey ms (t, o) = case o of
    Variable        -> Nothing
    Const n         -> Just n
    Operator op a b -> case op of
        '+'             -> fmap (+)   mma <*> mmb
        '-'             -> fmap (-)   mma <*> mmb
        '*'             -> fmap (*)   mma <*> mmb
        '/'             -> fmap (div) mma <*> mmb
        '='             -> case (mma, mmb) of
            (Nothing, Just n) -> solveForMonkey ms ma n
            (Just n, Nothing) -> solveForMonkey ms mb n
            otherwise         -> Nothing
        otherwise       -> error $ "Operator '" ++ [op] ++ "'' not implemented"
        where 
            mma = (resolveMonkey ms ma)
            mmb = (resolveMonkey ms mb)
            ma  = fromJust $ findMonkey ms a
            mb  = fromJust $ findMonkey ms b

-- Solves for a single monkey given a value
solveForMonkey :: [Monkey] -> Monkey -> Int -> Maybe Int
solveForMonkey ms (t, o) y = case o of
    Variable        -> Just y
    Const n         -> Just n 
    Operator op a b -> case (mma, mmb) of
        (Nothing, Nothing)      -> error $ "Cannot solve this equation, too many variables"
        (Just na,  Nothing)     -> case op of
            '+' -> solveForMonkey ms mb (y - na)
            '-' -> solveForMonkey ms mb (na - y)
            '*' -> solveForMonkey ms mb (y `div` na)
            '/' -> solveForMonkey ms mb (na `div` y)
        (Nothing, Just nb)      -> case op of
            '+' -> solveForMonkey ms ma (y - nb) 
            '-' -> solveForMonkey ms ma (nb + y)
            '*' -> solveForMonkey ms ma (y `div` nb)
            '/' -> solveForMonkey ms ma (y * nb)
        where
            mma = (resolveMonkey ms ma)
            mmb = (resolveMonkey ms mb)
            ma  = fromJust $ findMonkey ms a
            mb  = fromJust $ findMonkey ms b

-- The solver for part #1 of the puzzle
solvePart1 :: [Monkey] -> Int
solvePart1 ms = case resolveMonkey ms rm of 
    Nothing -> error "Failed to resolve monkey yell"
    Just n  -> n
    where
        rm = fromJust $ findMonkey ms "root"

-- The solver for part #2 of the puzzle
solvePart2 :: [Monkey] -> Int
solvePart2 ms = case resolveMonkey ms' rm' of 
    Nothing -> error "Failed to resolve monkey yell"
    Just n  -> n
    where
        ms'                     = foldl replaceMonkey ms [rm', ym']
        ym'                     = (ty, Variable)
        rm'                     = (tr, Operator '=' a b)
        ym@(ty, _)              = fromJust $ findMonkey ms "humn"
        rm@(tr, Operator _ a b) = fromJust $ findMonkey ms "root"

-- The full solver
day21Solver :: IO [Int]
day21Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
