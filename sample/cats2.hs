{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Applicative ((<*>))
import Data.OI hiding (openFile)
import System.IO (IOMode (..), Handle)
import qualified System.IO as IO
import Prelude hiding (readFile, writeFile)

main :: IO ()
main = runInteraction pmain

pmain :: (([String],Either ((Handle, String), [(Handle, String)]) [(Handle, String)]), (Handle, ())) :-> ()
pmain = args 
    |/| ((choiceOI (lines . readFile "files2" |/| zipWithOI readFile) . zipWithOI readFile) <*> null)
    |/| writeFile "output2.txt" . concatMap textproc

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
