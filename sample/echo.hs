{-# LANGUAGE TypeOperators #-}
module Main where

import Data.OI hiding (isEOF)

import Data.Maybe
import System.IO

import qualified System.Environment as Sys (getArgs)

main :: IO ()
main =  runInteraction pmain

pmain :: ([String], Either [(Maybe Char, Maybe ())] ([Maybe Char], [Maybe ()])) :-> ()
pmain = null . getArgs |/| choiceOI pmain0 pmain1

pmain0 :: [(Maybe Char, Maybe ())] :-> ()
pmain0 = forceSeq . takeWhile isJust . mapOI echochar

pmain1 :: ([Maybe Char], [Maybe ()]) :-> ()
pmain1 = forceSeq . echostring

getc :: Maybe Char :-> Maybe Char
getc = iooi getchar

putc :: Maybe Char -> Maybe () :-> Maybe ()
putc = iooi . putchar

getchar :: IO (Maybe Char)
getchar = choice (return Nothing) (return . Just =<< getChar) =<< isEOF 

putchar :: Maybe Char -> IO (Maybe ())
putchar = maybe (return Nothing)
                ((return . Just =<<) . putChar)

gets :: [Maybe Char] :-> String
gets = map fromJust . takeWhile isJust . mapOI getc

puts :: String -> [Maybe ()] :-> [Maybe ()]
puts = zipWithOI putc . map Just

echochar :: (Maybe Char, Maybe ()) :-> Maybe ()
echochar  = getc |/| putc

echostring :: ([Maybe Char], [Maybe ()]) :-> [Maybe ()]
echostring = gets |/| puts

getArgs :: [String] :-> [String]
getArgs = iooi Sys.getArgs

choice :: a -> a -> Bool -> a
choice t f c = if c then t else f
