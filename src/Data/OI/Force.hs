-- |
-- Module      : Data.OI.Force
-- Copyright   : (c) Nobuo Yamashita 2012-2016
-- License     : BSD3
-- Author      : Nobuo Yamashita
-- Maintainer  : nobsun@sampou.org
-- Stability   : experimental
--
module Data.OI.Force (
  -- * Forcing utility functions
   force
  ,force'
  ,forceSeq
  ) where

import Control.Parallel

-- | forces sequence
forceSeq :: [a] -> ()
forceSeq = force . dropWhile (()==) . map force

-- | forces
force :: a -> ()
force x = x `pseq` ()

-- | returns forcing invoker
force' :: a -> (a,())
force' x = x `pseq` (x,())
