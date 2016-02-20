-- |
-- Module      : Data.OI.Combinator
-- Copyright   : (c) Nobuo Yamashita 2011-2016
-- License     : BSD3
-- Author      : Nobuo Yamashita
-- Maintainer  : nobsun@sampou.org
-- Stability   : experimental
--
{-# LANGUAGE TypeOperators #-}
module Data.OI.Combinator
  (
   -- * Interaction Combinators
   (|:|)
  ,(|>|),(|/|)
  ,(|><|)
   -- * Iteration
  ,sequenceOI
  ,foldOI
  ,mapOI
  ,zipWithOI
  ,zipWithOI'
   -- * Conditional Choice
  ,ifOI
  ,choiceOI
  ,choiceOIOn
   -- * Sequencing
  ,seqsOI
  ,seqsOI'
  ) where

import Data.OI.Internal
import Data.OI.Force

-- | Connect two interactions into an interaction

infixl 3 |:|
infixl 2 |>|,|/|
infixl 1 |><|

(|:|) :: (a :-> c) -> (b :-> d) -> ((a,b) :-> (c,d))
(f |:| g) o = case dePair o of (a,b) -> (f a, g b)

(|/|) :: (a :-> c) -> (c -> (b :-> d)) -> ((a,b) :-> d)
(f |/| g) o = case dePair o of (a,b) -> g (f a) b

(|>|) :: (a :-> (p,c)) -> (b :-> (p -> d)) -> ((a,b) :-> (c,d))
(f |>| g) o = case dePair o of (a,b) -> (c, g b p) where (p,c) = f a

(|><|) :: (a :-> (p -> (q,c)))
       -> (b :-> (q -> (p,d)))
       -> ((a,b) :-> (c,d))
(f |><| g) o = case dePair o of
  (a,b) -> (c,d) where (q,c) = f a p; (p,d) = g b q

-- | Iteration

foldOI :: (a :-> (b -> b)) -> b -> ([a] :-> b)
foldOI op z xxs = case deList xxs of
  Just (x,xs) -> x `op` foldOI op z xs
  _           -> z

sequenceOI :: [a :-> b] -> [a] :-> [b]
sequenceOI (f:fs) oos = case deList oos of
  Just (o,os) -> f o : sequenceOI fs os
  _           -> []
sequenceOI _ _ = []

mapOI :: (a :-> b) -> ([a] :-> [b])
mapOI f = sequenceOI (repeat f)

zipWithOI :: (a -> (b :-> c)) -> ([a] -> ([b] :-> [c]))
zipWithOI _ [] _ = []
zipWithOI f (b:bs) os = case deList os of
  Just (x,xs) -> f b x : zipWithOI f bs xs
  _           -> []

zipWithOI' :: (a :-> (b -> c)) -> ([a] :-> ([b] -> [c]))
zipWithOI' = flip . zipWithOI . flip

-- | Conditional branching

ifOI :: Bool -> (a :-> c) -> (b :-> c) -> (Either a b :-> c)
ifOI True  t _ o = case deLeft o of
  Left x  -> t x
  _       -> error "ifOI: Left expected but Right"
ifOI False _ e o = case deRight o of
  Right y -> e y
  _       -> error "ifOI: Right expected but Left"

choiceOI :: (a :-> c) -> (b :-> c) -> Bool -> (Either a b :-> c)
choiceOI = flip . flip ifOI

choiceOIOn :: (t -> a :-> c) -> (t -> b :-> c) -> (t -> Bool)
           -> t -> Either a b :-> c
choiceOIOn f g p x = choiceOI (f x) (g x) (p x)

-- | Sequencing

seqsOI :: [a :-> b] -> ([a] :-> ())
seqsOI s os =  forceSeq $ zipWithOI ($) s os

seqsOI' :: [a] :-> ([a :-> b] -> ())
seqsOI' os is = case dropWhile (()==) $ map force $ zipWithOI id is os of
  [] -> ()
  _  -> error "seqsOI': Impossible!"
