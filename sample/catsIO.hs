module Main where

import System.Environment
import System.IO hiding (readFile, writeFile)
import Prelude hiding (readFile, writeFile)

{-
 If "files" contains much many files, "./catsIO" throws a resource-exhaused exception."
-}

main :: IO ()
main = do
  args     <- getArgs
  contents <- if not $ null args 
                then do
                  mapM readFile args
                else do 
                  files <- readFile "files"
                  mapM readFile (lines files)
  writeFile "output.txt" (concatMap textproc contents)

textproc :: String -> String
textproc = unlines . take 1 . lines

readFile :: String -> IO String
readFile f = do
  h  <- openFile f ReadMode
  cs <- hGetContents h
  return cs

writeFile :: String -> String -> IO ()
writeFile f cs = do
  h  <- openFile f WriteMode
  r  <- hPutStr h cs
  return r
