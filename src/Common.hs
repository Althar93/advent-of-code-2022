module Common (
    executeAndPrintResults,
    removeDuplicates,
    chunksOf,
    consMaybe,
    concatMaybe,
    mapJusts,
    minmax,
    isUnique,
    rotate
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

-- Constructs a maybe list by appending a maybe value to a maybe list, yielding Nothing if either is Nothing
consMaybe :: Maybe a -> Maybe [a] -> Maybe [a]
consMaybe x ys = fmap (:) x <*> ys

-- Concatenates two maybe lists by appending their just values together
concatMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
concatMaybe xs ys = fmap (++) xs <*> ys

-- Maps a function over a list of maybes on the just values only
mapJusts :: (a -> b) -> [Maybe a] -> [Maybe b]
mapJusts _ []           = []
mapJusts f (mx:xs)      = case mx of
    Nothing -> Nothing:(mapJusts f xs)
    Just x  -> (Just (f x)):(mapJusts f xs)

-- Returns the min max of two values
minmax :: (Ord a) => a -> a -> (a, a)
minmax a b | a <= b     = (a, b)
           | otherwise  = (b, a)

-- Returns whether an element is unique or nothing if the element could not be found
isUnique :: (Eq a) => a -> [a] -> Maybe Bool
isUnique y xs = isUnique' y xs Nothing where
    isUnique' _ []     m = m
    isUnique' y (x:xs) m    | (y /= x)  = isUnique' y xs m
                            | otherwise = case m of
                                Nothing     -> isUnique' y xs (Just True)
                                otherwise   -> Just False

-- Rotates the list to the left by the specified amount
rotate :: Int -> [a] -> [a]
rotate n xs = (drop n' xs) ++ (take n' xs) where
    n' = n `mod` length xs