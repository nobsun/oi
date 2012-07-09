{-# LANGUAGE TypeOperators #-}
module Main where

import Data.OI hiding (isEOF)

import Data.Char
import System.IO

main :: IO ()
main = runInteraction $ pmain1

pmain0 = forceSeq . mapOI echo
pmain1 = forceSeq . echos

eof :: Int
eof = -1

getc :: Int :-> Int
getc = iooi getchar

putc :: Char -> () :-> ()
putc = iooi . putChar

putc' :: Int -> () :-> ()
putc' (-1) = const ()
putc' c    = iooi (putChar (chr c))

gets :: [Int] :-> String
gets = map chr . takeWhile (eof /=) . mapOI getc

puts :: String -> [()] :-> [()]
puts = zipWithOI putc

getchar :: IO Int
getchar = choice (return (-1)) (return . ord =<< getChar) =<< isEOF 

puts' :: String -> OI [()] -> ()
puts' = seqsOI . map putc

echo  = getc |/| putc'
echos = gets |/| puts