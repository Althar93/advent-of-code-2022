module Day7 (day7Solver) where

import Parser
import Data.Char
import Data.List

-- The input file path
inputFile :: FilePath
inputFile = "res/day7_input.txt"

-- List of terminal outputs
data Output = OutDirectory String | OutFile String Int deriving (Show)

-- List of terminal commands
data Command = CmdCd String | CmdLs deriving (Show)

-- Basic file system
data FileSystem = Directory String [FileSystem] | File String Int deriving (Show, Eq)

-- A file system state
data FileSystemState = FileSystemState { current :: [String], fileSystem :: FileSystem } deriving (Show)

-- Parses a token name
parseTokenName :: Parser String
parseTokenName = some $ parseIs isAlphaNum

-- Parses a file name
parseFileName :: Parser String
parseFileName = some $ parseIs (\x -> isAlphaNum x || x == '.')

-- Parses a directory name
parseDirectoryName :: Parser String
parseDirectoryName = some $ parseIs (\x -> isAlphaNum x || x == '/' || x == '.')

-- Parses a single command
parseCommand :: Parser Command
parseCommand = do
  parseChar '$'
  parseSpaces
  c <- parseTokenName
  case c of
    "cd"       -> do 
          parseSpaces
          d <- parseDirectoryName
          return $ CmdCd d 
    "ls"       -> return CmdLs
    otherwise  -> error "Command not recognised"
    
-- Parses a single file
parseFile :: Parser Output
parseFile = do
  s <- parseInt
  parseSpaces
  n <- parseFileName
  return $ OutFile n s

-- Parses a single directory
parseDirectory :: Parser Output
parseDirectory = do
  parseString "dir"
  parseSpaces
  n <- some (parseIs isAlphaNum)
  return $ OutDirectory n

-- Parses an output
parseOutput :: Parser Output
parseOutput = parseDirectory <|> parseFile

-- Executes a single file system command
executeCommand :: Command -> FileSystemState -> FileSystemState
executeCommand (CmdCd n) fss = case n of
    "/"  -> fss { current = ["/"]                }
    ".." -> fss { current = (tail (current fss)) }
    n    -> fss { current = (n:(current fss))    }
executeCommand (CmdLs)   fss = fss

-- Executes a single file system
executeOutput :: Output -> FileSystemState -> FileSystemState
executeOutput (OutDirectory n) fss = fss { fileSystem = fs' } where
    fs' = insertDirectory (current fss) n (fileSystem fss)
executeOutput (OutFile n s) fss    = fss { fileSystem = fs' } where
    fs' = insertFile (current fss) n s (fileSystem fss)

-- Returns whether the file system is a directory
isDirectory :: FileSystem -> Bool
isDirectory (Directory _ _) = True
isDirectory _               = False

-- Inserts a file system item (if it does not exist)
insertAt :: [String] -> FileSystem -> FileSystem -> FileSystem
insertAt cs ifs (Directory d xs) | (null cs)                                                 = Directory d xs
                                 | (last cs) == d && (length cs) == 1 && not (ifs `elem` xs) = Directory d (ifs:xs)
                                 | (last cs) == d && (length cs) >  1                        = Directory d (map (insertAt (init cs) ifs) xs)
                                 | otherwise                                                 = Directory d xs
insertAt _  _   fs                                                                           = fs

-- Inserts a directory
insertDirectory :: [String] -> String -> FileSystem -> FileSystem
insertDirectory cs n fs = insertAt cs (Directory n []) fs

-- Inserts a file
insertFile :: [String] -> String -> Int -> FileSystem -> FileSystem
insertFile cs n s fs = insertAt cs (File n s) fs
  
-- Parses the file system
parseFileSystem :: Parser FileSystem
parseFileSystem = do
    s <- many $ pLine (pEither parseCommand parseOutput)
    return $ fileSystem (foldl replayFileSystemState defaultFileSystemState s) where
        replayFileSystemState fss (Left  c) = executeCommand c fss
        replayFileSystemState fss (Right o) = executeOutput  o fss
        defaultFileSystemState              = FileSystemState { current = ["/"], fileSystem = (Directory "/" []) }

-- Reads the test input
readInputs :: IO FileSystem
readInputs = do
    contents <- readFile inputFile
    return $ (runParser parseFileSystem contents)

-- Computes the total size of a file system (directory and/or file(s))
totalFileSystemSize :: FileSystem -> Int
totalFileSystemSize (File _ s)       = s
totalFileSystemSize (Directory _ xs) = sum $ map totalFileSystemSize xs

-- Traverses the filesytem given a predicate & function to execute
traverseFileSystem :: (FileSystem -> Bool) -> (FileSystem -> a) -> FileSystem -> [a]
traverseFileSystem p f fss = traverseFileSystem' p f [] fss where
    traverseFileSystem' p f rs x@(Directory _ xs) = if (p x) then ((f x) : concatMap (traverseFileSystem' p f rs) xs) else concatMap (traverseFileSystem' p f rs) xs
    traverseFileSystem' p f rs x@(File _ _)       = if (p x) then ((f x) : rs) else rs

-- List the sizes of the directories within the file system
listDirectorySizes :: FileSystem -> [Int]
listDirectorySizes fs = traverseFileSystem isDirectory totalFileSystemSize fs

-- The solver for part #1 of the puzzle
solvePart1 :: FileSystem -> Int
solvePart1 fs = sum $ filter (<= 100000) ds where
    ds  = listDirectorySizes fs

-- The solver for part #2 of the puzzle
solvePart2 :: FileSystem -> Int
solvePart2 fs = head $ filter (>= ms) dss where
    ms =  30000000 - (70000000 - (last dss))
    dss = sort ds
    ds  = listDirectorySizes fs

-- The full solver
day7Solver :: IO [Int]
day7Solver = do
    input <- readInputs
    return [solvePart1 input, solvePart2 input]