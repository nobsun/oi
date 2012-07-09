{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Data.OI.IO
-- Copyright   : (c) Nobuo Yamashita 2011-2012
-- License     : BSD3
-- Author      : Nobuo Yamashita
-- Maintainer  : nobsun@sampou.org
-- Stability   : experimental
--
module Data.OI.IO
  (
  -- * Interaction enable to handle I/O error
   (:~>)
  -- * I/O oprations
  ,openFile
  ,hIsClosed
  ,hIsEOF
  ,hGetLine
  ,hClose
  ,hPutStrLn
  ,isEOF
  ,getLine
  ,putStrLn
  ) where

import Data.OI.Internal

import System.IO (IOMode(..),Handle)
import qualified System.IO as IO
import System.FilePath
import Prelude hiding (getLine,putStrLn)

type a :~> b = OI (IOResult a) -> IOResult b
infixr 0 :~>

openFile :: FilePath -> IOMode -> Handle :~> Handle
openFile = (iooi' .) . IO.openFile

hIsClosed :: IO.Handle -> Bool :~> Bool
hIsClosed = iooi' . IO.hIsClosed

hIsEOF :: IO.Handle -> Bool :~> Bool
hIsEOF = iooi' . IO.hIsEOF

hGetLine :: IO.Handle -> String :~> String
hGetLine = iooi' . IO.hGetLine

hClose :: IO.Handle -> () :~> ()
hClose = iooi' . IO.hClose

hPutStrLn :: IO.Handle -> String -> () :~> ()
hPutStrLn = (iooi' .) . IO.hPutStrLn

isEOF :: Bool :~> Bool
isEOF = iooi' IO.isEOF

getLine :: String :~> String
getLine = iooi' IO.getLine

putStrLn :: String -> () :~> ()
putStrLn = iooi' . IO.putStrLn
