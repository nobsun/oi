{-# LANGUAGE TypeOperators
  #-}
module Main where

import Data.OI

import Control.Concurrent
import Control.Parallel
import System.Environment

main :: IO ()
main = do { kbd1:scr1:kbd2:scr2:_ <- getArgs
          ; runInteraction $ pmain (kbd1,scr1) (kbd2,scr2)
          }

pmain ::  (FilePath,FilePath)
      ->  (FilePath,FilePath)
      ->  ((String, [String], ()), (String, [String], ())) :-> ()
pmain devs1 devs2 = uncurry par . (talk "Alice" devs1 |><| talk "Bob" devs2)

talk :: String                -- Talker's name
     -> (FilePath,FilePath)   -- (Talker's keybord, Talker's screen)
     -> (String,[String],())  -- (Talker's input,Merged messages, Process result)
    :-> [String]             -- Messages from the other end
     -> ([String],())        -- (Messages to the other end, Process result)
talk name (kbd,scr) r msg = (ins,showscr scr (mergeOI ins msg os) us)
  where
    (is,os,us) = deTriple r
    ins = map ((name ++ ": ")++) $ lines $ readkbd kbd is

readkbd :: FilePath -> String :-> String
readkbd = iooi . readFile

showscr :: FilePath -> [String] -> () :-> ()
showscr s = iooi . writeFile s . unlines

mergeOI = (iooi .) . mergeIO