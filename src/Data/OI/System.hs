{-# LANGUAGE TypeOperators
  #-}
-- |
-- Module      : Data.OI.System
-- Copyright   : (c) Nobuo Yamashita 2011-2016
-- License     : BSD3
-- Author      : Nobuo Yamashita
-- Maintainer  : nobsun@sampou.org
-- Stability   : experimental
--
module Data.OI.System 
  (
   args
  ,progName
  ,environment
  )
 where

import System.Environment
import Data.OI.Internal

args :: [String] :-> [String]
args = iooi getArgs

progName :: String :-> String
progName = iooi getProgName

environment :: [(String, String)] :-> [(String, String)]
environment = iooi getEnvironment


