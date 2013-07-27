{-# LANGUAGE TypeOperators #-}
module Main where

import Data.OI
import System.IO

main :: IO ()
main = do 
  hSetBuffering stdin NoBuffering
  runInteraction morec
  Prelude.putStrLn ""

(|?|) :: (a :-> c)
      -> (b :-> c)
      -> Bool
      -> (Either a b :-> c)
(|?|) = choiceOI

getc :: Char :-> Char
getc  = iooi getChar

putc :: Char -> (() :-> ())
putc  = iooi . putChar

echoc :: (Char,()) :-> ()
echoc = getc |/| putc

morec :: (Char, Either (Char, ()) ()) :-> ()
morec = ('e' ==) . getc |/| (echoc |?| putc 'q')
