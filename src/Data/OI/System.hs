{-# LANGUAGE TypeOperators
  #-}

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


