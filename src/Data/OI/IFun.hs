{-# LANGUAGE TypeOperators
            ,BangPatterns
  #-}
-- |
-- Module      : Data.OI.IFun
-- Copyright   : (c) Nobuo Yamashita 2012
-- License     : BSD3
-- Author      : Nobuo Yamashita
-- Maintainer  : nobsun@sampou.org
-- Stability   : experimental
--
module Data.OI.IFun
  (
  -- * Type of function with interaction
   IFun
  -- * IFun combinator
  ,(|::|)
  ,(|->|)
  ,(|<>|)
  ) where

import Data.OI.Internal

type IFun p a b = a -> p :-> b

(|::|) :: IFun p a c -> IFun q  b d -> IFun (p,q) (a,b) (c,d)
(f |::| g) (a,b) opq = case dePair opq of
  (p,q) -> (f a p, g b q)

(|->|) :: IFun p a (b',c) -> IFun q (b',b) d -> IFun (p,q) (a,b) (c,d)
(f |->| g) (a,b) opq = case dePair opq of
  (p,q) -> case f a p of
    (b',c) -> (c, g (b',b) q)

(|<>|) :: IFun p (a',a) (b',c) -> IFun q (b',b) (a',d) -> IFun (p,q) (a,b) (c,d)
(f |<>| g) (a,b) opq = case dePair opq of
  (p,q) -> (c,d) where (b',c) = f (a',a) p; (a',d) = g (b',b) q 
