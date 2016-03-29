{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Arrow (second)
import Data.OI hiding (openFile)
import System.IO (IOMode (..), Handle)
import qualified System.IO as IO
import Prelude hiding (readFile, writeFile)

main :: IO ()
main = runInteraction pmain

{- -
pmain :: ((([String], Either (IOResult (Resource String)) b),[IOResult (Resource String)]),IOResult (Resource ())) :-> ()
pmain = inputs |/| outputs

inputs :: (([String], Either (IOResult (Resource String)) b), [IOResult (Resource String)]) :-> [IOResult (Resource String)]
inputs = args |/| choiceOIOn (const $ fileList "files") const null |/| zipWithOI inFileResource

fileList :: FilePath -> (IOResult (Resource String)) :-> [String]
fileList f = mapR id . inFileResource f

outputs :: [IOResult (Resource String)] -> IOResult (Resource ()) :-> ()
outputs = (closeR .) . outFileResource "output.txt" . concatMap (mapR textproc)
-- -}

{- -}
pmain0 = useUpR . inFileResource "input.txt"
     |/| maybe (const (Failure "fail in inputing")) (outFileResource "output.txt")
     |/| const . closeR
-- -}

{- -}
textproc :: String -> String
textproc = id

readFile :: FilePath -> (Handle, String) :-> String
readFile f      = openFile f ReadMode   |/| hGetContents

writeFile :: FilePath -> String -> (Handle, ()) :-> ()
writeFile f cs  = openFile f WriteMode  |/| flip hPutStr cs

openFile :: FilePath -> IOMode -> Handle :-> Handle
openFile     = (iooi .) . IO.openFile

hGetContents :: Handle -> String :-> String
hGetContents = iooi . IO.hGetContents

hPutStr :: Handle -> String -> () :-> ()
hPutStr      = (iooi .) . IO.hPutStr

pmain :: (([String],Either ((Handle, String), [(Handle, String)]) [(Handle, String)]), (Handle, ())) :-> ()
pmain = args |/| choiceOIOn (const $ lines . readFile "files" |/| zipWithOI readFile) 
                             (zipWithOI readFile)
                             null
              |/| writeFile "output.txt" . concatMap textproc
-- -}


