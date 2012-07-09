module Main where

import System.Environment
import qualified System.IO as IO
import System.IO.Strict hiding (readFile, writeFile)
import Prelude hiding (readFile, writeFile)

{-
 If the amount of size of files contained in "files" is too much, "./catsSIO" throws a out-of-memory exception."
-}

main :: IO ()
main = do
  args     <- getArgs
  main' args

main' :: [String] -> IO ()
main' args = run $ do
  contents <- if not $ null args 
                then do
                  mapM readFile args
                else do 
                  files <- readFile "files"
                  mapM readFile (lines files)
  writeFile "output.txt" (concatMap textproc contents)

textproc :: String -> String
textproc = unlines . take 1 . lines

readFile :: String -> SIO String
readFile f = do
  h  <- openFile f IO.ReadMode
  cs <- hGetContents h
  return cs

writeFile :: String -> String -> SIO ()
writeFile f cs = do
  h  <- openFile f IO.WriteMode
  r  <- hPutStr h cs
  return r
