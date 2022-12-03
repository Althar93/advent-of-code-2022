module Common (
    executeAndPrintResults,
    removeDuplicates,
    chunksOf
    ) where

import Data.Time
import Data.List

-- Convenience function for executing & printing the result of a puzzle
executeAndPrintResults :: (Show a) => String -> IO a -> IO ()
executeAndPrintResults title solver = do
    startTime <- getCurrentTime
    result <- solver
    putStrLn $ title ++ " : " ++ show result
    endTime <- getCurrentTime
    putStrLn $ "Took " ++ (show (diffUTCTime endTime startTime)) ++ " to execute."

-- Removes the duplicate items from a list
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

-- Splits a list into regularly sized chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf 0 _  = []
chunksOf n xs = (take n xs) : (chunksOf n (drop n xs))
