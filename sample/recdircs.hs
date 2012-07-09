{-# LANGUAGE TupleSections  
            ,PostfixOperators
            ,TypeOperators
            #-}
module Main where

import Control.Arrow
import Data.List
import Data.OI
import System.FilePath
import System.Directory

main :: IO ()
main = runInteraction pmain

pmain :: (([String], [(Bool, IOResult [FilePath])]), ()) :-> ()
pmain = (((args >>> head) |/| getDirCsRec) >>> snd) |/| putStrOI . unlines


putStrOI :: String -> () :-> ()
putStrOI = iooi . putStr

isDirectory :: FilePath -> Bool :-> Bool
isDirectory = iooi . doesDirectoryExist

getDirCs' :: FilePath -> IOResult [FilePath] :-> [FilePath]
getDirCs' fp = iooi'  (getDirectoryContents fp) >>> valid

valid :: IOResult [FilePath] -> [FilePath]
valid (Success fps) = filter (flip notElem [".",".."]) fps
valid _             = []

getDirCs :: FilePath -> (Bool, IOResult [FilePath]) :-> (Bool, [FilePath])
getDirCs fp = isDirectory fp
          |/| choice (((,) True) . map (fp </>) . getDirCs' fp) (const (False, [fp]))

getDirCsRec ::  FilePath
            ->  [(Bool, IOResult [FilePath])]
            :-> (OI [(Bool, IOResult [FilePath])], [FilePath])
getDirCsRec fp oss = case deList oss of
  Just (o,os) -> case getDirCs fp o of
    (False, fps) -> (os,fps)
    (True,  fps) -> second concat $ mapAccumL (flip getDirCsRec) os fps
  _           -> error $ "getDirCs: perhaps `"++ fp ++"' is not found"
