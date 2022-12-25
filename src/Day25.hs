module Day25 (day25Solver) where

import Common
import Parser

-- A SNAFU number
type SNAFUNumber = [Char]

-- The input file path
inputFile :: FilePath
inputFile = "res/day25_input.txt"

parseSNAFUNumber :: Parser SNAFUNumber
parseSNAFUNumber = do
    parseSpaces
    xs <- some $ parseIsOneOf "012=-"
    return $ xs

-- Parses SNAFU numbers
parseSNAFUNumbers :: Parser [SNAFUNumber]
parseSNAFUNumbers = some $ parseSNAFUNumber

-- Reads the test input
readInputs :: IO [SNAFUNumber]
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseSNAFUNumbers contents)

-- Converts the SNAFU number to decimal
convertToDecimal :: SNAFUNumber -> Int
convertToDecimal ns = foldl (\a x -> a * 5 + convertToDigit x) 0 ns where
    convertToDigit '=' = -2 
    convertToDigit '-' = -1
    convertToDigit '0' =  0
    convertToDigit '1' =  1
    convertToDigit '2' =  2
    convertToDigit  c  = error $ "Digit " ++ [c] ++ " not supported"

-- Converts the decimal back to a SNAFU number
convertToSNAFUNumber :: Int -> SNAFUNumber
convertToSNAFUNumber d | d' > 0     = convertToSNAFUNumber d' ++ convertToSNAFUDigit r 
                       | otherwise  = convertToSNAFUDigit r
                        where
                            d'      = m + ([0, 0, 0, 1, 1] !! r)
                            (m, r)  = (d `div` 5, d `mod` 5)
                            convertToSNAFUDigit 0 = "0"
                            convertToSNAFUDigit 1 = "1"
                            convertToSNAFUDigit 2 = "2"
                            convertToSNAFUDigit 3 = "="
                            convertToSNAFUDigit 4 = "-"

-- The solver for part #1 of the puzzle
solvePart1 :: [SNAFUNumber] -> SNAFUNumber
solvePart1 ns = convertToSNAFUNumber nt where
    nt = sum $ map convertToDecimal ns

-- The full solver
day25Solver :: IO [SNAFUNumber]
day25Solver = do
    input <- readInputs
    return [solvePart1 input]
