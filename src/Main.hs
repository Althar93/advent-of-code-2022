module Main where

import Common
import Day1
import Day2
import Day3
import Day4
import Day5

main :: IO ()
main = do
    putStrLn "== Advent of Code 2022 =="
    executeAndPrintResults "Day 1" day1Solver
    executeAndPrintResults "Day 2" day2Solver
    executeAndPrintResults "Day 3" day3Solver
    executeAndPrintResults "Day 4" day4Solver
    executeAndPrintResults "Day 5" day5Solver
