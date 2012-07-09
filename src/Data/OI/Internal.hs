{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-cse #-}
-- |
-- Module      : Data.OI.Base
-- Copyright   : (c) Nobuo Yamashita 2011-2012
-- License     : BSD3
-- Author      : Nobuo Yamashita
-- Maintainer  : nobsun@sampou.org
-- Stability   : experimental
--
module Data.OI.Internal
  (
   -- * Types
   OI
  ,(:->)
  -- * Primitive operators on OI
  ,(??)
  -- * Splitters against `OI' datatype
  ,dePair
  ,deList
  ,deTriple
  ,deTuple4
  ,deTuple5
  ,deTuple6
  ,deTuple7
  ,deLeft
  ,deRight
  -- * Interaction driver
  ,runInteraction
  -- * IO converters
  ,IOResult(..)
  ,iooi
  ,iooi'
  ) 
  where

import Control.Applicative
import Control.Category
import Control.Comonad
import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Parallel
import System.IO.Unsafe
import Prelude hiding ((.),id,catch)

-- | Datatype for intermediating interaction: 
-- @OI@ has two states (programmer cannot distinguish), non-expressed and exressed.
-- `Non-expressed' indicates that no computation is assigned.
-- In other words, it's value is never denotated by any expression.
-- So, if you refer the value then the process will be suspended 
-- until other process determins the value.
-- Non-expressed value can be determined to become 'expressed' for a value by a expression at most once.
-- `Expressed' indicates that some computation is assigned for the value.
-- Once expressed, the value never be back to non-expressed nor be changed.
data OI a = OI (LeftValueOf a) (RightValueOf a)

-- | Interaction (a function from a intermediating type to another type) type
type a :-> b = OI a -> b
infixr 0 :->

instance Functor OI where
  fmap f = (##) . f . (??)

instance Monad OI where
  return = (##)
  (>>=)  = flip ($) . (??)

instance Comonad OI where
  extract = (??)
  duplicate = (##)

-- | Dereference operator
(??) :: OI a -> a
(??) (OI _ val) = val

-- | Reference operator
(##) :: a -> OI a
(##) x = OI (unsafeNew x) x

-- | Decomposer for pair
dePair :: OI (a,b) -> (OI a, OI b)
dePair (OI vxy ~(x,y)) = put io vxy `pseq` (OI vx x, OI vy y)
  where
    vx  = new x
    vy  = new y
    io  = (,) <$> lazy (deref vx) <*> lazy (deref vy)

-- | Decomposer for list
deList :: OI [a] -> Maybe (OI a, OI [a])
deList (OI vxxs xxs) = put io vxxs
  `pseq` case xxs of
     x:xs -> Just (OI vx x, OI vxs xs)
     _    -> Nothing
  where
    vx  = new (undefined :: a)
    vxs = new (undefined :: [a])
    io = (:) <$> lazy (deref vx) <*> lazy (deref vxs)

-- | Decomposer for triple
deTriple :: OI (a,b,c) -> (OI a, OI b, OI c)
deTriple (OI vxyz ~(x,y,z)) = put io vxyz
  `pseq` (OI vx x, OI vy y, OI vz z)
  where
    vx  = new x
    vy  = new y
    vz  = new z
    io  = (,,) <$> lazy (deref vx) 
               <*> lazy (deref vy)
               <*> lazy (deref vz)

-- | Decomposer for 4-tuple
deTuple4 :: OI (a,b,c,d) -> (OI a, OI b, OI c, OI d)
deTuple4 (OI vwxyz ~(w,x,y,z)) = put io vwxyz
  `pseq` (OI vw w, OI vx x, OI vy y, OI vz z)
  where
    vw  = new w
    vx  = new x
    vy  = new y
    vz  = new z
    io  = (,,,) 
      <$> lazy (deref vw) 
      <*> lazy (deref vx) 
      <*> lazy (deref vy)
      <*> lazy (deref vz)

-- | Decomposer for 5-tuple
deTuple5 :: OI (a,b,c,d,e) -> (OI a, OI b, OI c, OI d, OI e)
deTuple5 (OI vvwxyz ~(v,w,x,y,z)) = put io vvwxyz
  `pseq` (OI vv v, OI vw w, OI vx x, OI vy y, OI vz z)
  where
    vv  = new v
    vw  = new w
    vx  = new x
    vy  = new y
    vz  = new z
    io  = (,,,,) 
      <$> lazy (deref vv) 
      <*> lazy (deref vw) 
      <*> lazy (deref vx) 
      <*> lazy (deref vy)
      <*> lazy (deref vz)

-- | Decomposer for 6-tuple
deTuple6 :: OI (a,b,c,d,e,f) -> (OI a, OI b, OI c, OI d, OI e, OI f)
deTuple6 (OI vuvwxyz ~(u,v,w,x,y,z)) = put io vuvwxyz
  `pseq` (OI vu u, OI vv v, OI vw w, OI vx x, OI vy y, OI vz z)
  where
    vu  = new u
    vv  = new v
    vw  = new w
    vx  = new x
    vy  = new y
    vz  = new z
    io  = (,,,,,) 
      <$> lazy (deref vu) 
      <*> lazy (deref vv) 
      <*> lazy (deref vw) 
      <*> lazy (deref vx) 
      <*> lazy (deref vy)
      <*> lazy (deref vz)

-- | Decomposer for 7-tuple
deTuple7 :: OI (a,b,c,d,e,f,g) -> (OI a, OI b, OI c, OI d, OI e, OI f, OI g)
deTuple7 (OI vtuvwxyz ~(t,u,v,w,x,y,z)) = put io vtuvwxyz
  `pseq` (OI vt t, OI vu u, OI vv v, OI vw w, OI vx x, OI vy y, OI vz z)
  where
    vt  = new t
    vu  = new u
    vv  = new v
    vw  = new w
    vx  = new x
    vy  = new y
    vz  = new z
    io  = (,,,,,,) 
      <$> lazy (deref vt) 
      <*> lazy (deref vu) 
      <*> lazy (deref vv) 
      <*> lazy (deref vw) 
      <*> lazy (deref vx) 
      <*> lazy (deref vy)
      <*> lazy (deref vz)

deLeft  :: OI (Either a b) -> Either (OI a) (OI b)
deLeft (OI ve ~(Left a)) = put io ve `pseq` Left (OI vl a)
  where
    vl = new a
    io = Left <$> lazy (deref vl)

deRight :: OI (Either a b) -> Either (OI a) (OI b)
deRight (OI ve ~(Right b)) = put io ve `pseq` Right (OI vr b)
  where
    vr = new b
    io = Right <$> lazy (deref vr)

-- | Drive interaction
runInteraction :: (OI a -> b) -> IO b
runInteraction pmain = do 
  { v <- newEmptyMVar
  ; x <- lazy (deref v)
  ; return $! pmain (OI v x)
  }

-- | @IOResult@ for error handling
data IOResult a = Success { result :: a }
                | Failure { errmsg :: String }

instance (Show a) => Show (IOResult a) where
  show (Success x) = show x
  show (Failure e) = e

instance Functor IOResult where
  fmap f (Success x) = Success (f x)
  fmap _ (Failure e) = Failure e

-- | Convert IO to interaction
iooi :: IO a -> OI a -> a
iooi io (OI var val) = put io var `pseq` val

iooi' :: IO a -> OI (IOResult a) -> IOResult a
iooi' io (OI var val)
  = put ( do { r <- try io
             ; case r of { Left  e -> return $ Failure (show (e :: SomeException))
                         ; Right a -> return $ Success a }}
        ) var `par` val

-- Wrapper

type LeftValueOf  a = MVar (IO a)
type RightValueOf a = a

new :: a -> LeftValueOf a
new   = unsafeNew

deref :: MVar a -> a
deref = unsafeDeref

put   :: a -> MVar a -> a
put   = unsafePut

lazy :: IO a -> IO a
lazy = unsafeInterleaveIO

-- Unsafe primitive

unsafeNew :: a -> LeftValueOf a
unsafeNew _ = unsafePerformIO newEmptyMVar

unsafeDeref :: MVar a -> a
unsafeDeref = unsafePerformIO . readMVar

unsafePut :: a -> MVar a -> a
unsafePut x v = unsafePerformIO $ do
              { s <- tryPutMVar v x
              ; if s then return x else readMVar v
              }

