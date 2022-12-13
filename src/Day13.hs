module Day13 (day13Solver) where

import Common
import Parser
import Data.List
import Data.Maybe

-- A packet
data Packet = List [Packet] | Single Int deriving (Eq)

-- Make packet an instance of show
instance Show Packet where
    show (Single n)         = (show n)
    show (List xs)          = "[" ++ (showElems xs) ++ "]" where
        showElems []        = ""
        showElems [x]       = show x
        showElems (x:xs)    = show x ++ "," ++ (showElems xs)

-- Make packet an instance of ordering
instance Ord Packet where
    compare (Single n0)  (Single n1) = compare n0 n1 
    compare (Single n0)  (List l1)   = compare (List [Single n0]) (List l1)
    compare (List l0)    (Single n1) = compare (List l0) (List [Single n1])
    compare (List l0)    (List l1)   = case dropWhile ((==)EQ) (zipWith (compare) l0 l1) of
        [] -> compare (length l0) (length l1)
        c  -> head c

-- A packet pair
type PacketPair = (Packet, Packet)

-- The input file path
inputFile :: FilePath
inputFile = "res/day13_input.txt"

-- Parses a packet recursively
parsePacket :: Parser Packet
parsePacket = do
    parseChar '['
    a <- many parseListItem
    parseChar ']'
    return $ List a 
    where
        parseSingle = do
            n <- parseInt
            return $ Single n
        parseListItem = do
            a <- parseSingle <|> parsePacket
            pMaybe $ parseChar ','
            return a

-- Parses a packet pair
parsePacketPair :: Parser PacketPair
parsePacketPair = do
    parseSpaces
    p0 <- parsePacket
    parseSpaces
    p1 <- parsePacket
    return (p0, p1)

-- Parses packet pairs
parsePacketPairs :: Parser [PacketPair]
parsePacketPairs = many $ parsePacketPair

-- Reads the test input
readInputs :: IO [PacketPair]
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parsePacketPairs contents)

-- The solver for part #1 of the puzzle
solvePart1 :: [PacketPair] -> Int
solvePart1 pp = sum $ map fst pp''' where
    pp'''    = filter (\(_, c) -> c == LT) pp''
    pp''     = zip [1..] pp'
    pp'      = map (\(p0, p1) -> p0 `compare` p1) pp

-- The solver for part #2 of the puzzle
solvePart2 :: [PacketPair] -> Int
solvePart2 pp = (fromJust (elemIndex d0 ps') + 1) * (fromJust (elemIndex d1 ps') + 1) where
    ps'     = sort (d0:d1:ps)
    ps      = foldl (\a (x, y) -> (x:y:a)) [] pp
    d0      = List [Single 2]
    d1      = List [Single 6]

-- The full solver
day13Solver :: IO [Int]
day13Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]
