{-# LANGUAGE TypeOperators #-}
module Main where

import Data.OI hiding (isEOF)

import Data.Bool
import Data.Maybe
import System.IO

import qualified System.Environment as Sys (getArgs)

main :: IO ()
main =  runInteraction pmain

pmain :: (Char, ()) :-> ()
pmain = getc |/| putc

getc :: Char :-> Char
getc = iooi (hSetBuffering stdin NoBuffering >> getChar)

putc :: Char -> () :-> ()
putc = iooi . (\c -> hSetBuffering stdout NoBuffering >> hPutChar stdout c >> hFlush stdout)
