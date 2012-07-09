module Main where

import System.Environment
import System.IO hiding (readFile, writeFile)
import System.IO.Unsafe
import Prelude hiding (readFile, writeFile)

main :: IO ()
main = do
  args     <- getArgs
  contents <- if not $ null args 
                then do
                  sequenceLz $ map readFile args
                else do 
                  files <- readFile "files"
                  sequenceLz $ map readFile (lines files)
  writeFile "output.txt" (concatMap textproc contents)

textproc :: String -> String
textproc = id -- unlines . take 1 . lines

lazy :: IO a -> IO a
lazy = unsafeInterleaveIO

mapLzM :: (a -> IO b) -> [a] -> IO [b]
mapLzM = (sequenceLz .) . map

sequenceLz :: [IO a] -> IO [a]
sequenceLz []     = lazy $ return []
sequenceLz (i:is) = lazy $ do
  r  <- lazy i
  rs <- sequenceLz is
  return (r:rs)

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
