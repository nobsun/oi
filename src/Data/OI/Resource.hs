{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Data.OI.Resource
-- Copyright   : (c) Nobuo Yamashita 2011-2016
-- License     : BSD3
-- Author      : Nobuo Yamashita
-- Maintainer  : nobsun@sampou.org
-- Stability   : experimental
--
module Data.OI.Resource
  (
  -- * Resource type
   Resource(..)
  -- * File resource
  ,inFileResource
  ,outFileResource
  -- * Resource handlers
  ,useUpR
  ,useUpRe
  ,closeR
  ,concatR
  ,foldR
  ,foldR'
  ,mapR
  ,mapR'
  ,filterR
  ,filterR'
  ,takeR
  ,takeR'
  ,takeWhileR
  ,takeWhileR'
  ) where

import Data.Maybe
import Control.Exception
import Data.OI.Internal
import System.IO (IOMode(..))
import qualified System.IO as IO
import System.IO.Unsafe
import Prelude hiding (readFile, writeFile)

data Resource a = Resource { release :: (), stream :: [IOResult a] }

instance (Show a) => Show (Resource a) where
  show r = show (stream r)

useUpR :: IOResult (Resource a) -> Maybe [a]
useUpR = foldR (:) []

useUpRe :: IOResult (Resource a) -> Maybe [a]
useUpRe = foldRe (:) []

closeR :: IOResult (Resource ()) -> ()
closeR result = case result of
  Success (Resource r rs) -> case dropWhile success rs of
    [] -> case r of {() -> ()}
    _  -> ()
  _                       -> ()
  where
    success r = case r of
      Success () -> True
      _          -> False

closeR' :: IOResult (Resource ()) -> Maybe ()
closeR' result = case result of
  Success (Resource r rs) -> case dropWhile success rs of
    [] -> case r of {() -> Just ()}
    _  -> Nothing
  _                       -> Nothing
  where
    success r = case r of
      Success () -> True
      _          -> False

concatR :: [IOResult (Resource a)] -> [a]
concatR = concat . mapMaybe (foldR (:) [])

foldR :: (a -> b -> b) -> b -> IOResult (Resource a) -> Maybe b
foldR f b result = case result of
  Success resource -> foldR' f b resource
  _                -> Nothing

foldRe :: (a -> b -> b) -> b -> IOResult (Resource a) -> Maybe b
foldRe f b result = case result of
  Success resource -> foldRe' f b resource
  _                -> Nothing

foldR' :: (a -> b -> b) -> b -> Resource a -> Maybe b
foldR' f b (Resource r (Success x:xs)) = return . f x =<< foldR' f b (Resource r xs)
foldR' f b (Resource r (Failure _:_))  = case r of () -> Just b
foldR' _ b (Resource r _)              = case r of () -> Nothing

foldRe' :: (a -> b -> b) -> b -> Resource a -> Maybe b
foldRe' f b (Resource r (Success x:xs)) = return . f x =<< foldRe' f b (Resource r xs)
foldRe' f b (Resource r (Failure _:_))  = case r of () -> Nothing
foldRe' _ b (Resource r _)              = case r of () -> Nothing

mapR :: (a -> b) -> IOResult (Resource a) -> [b]
mapR f (Success res) = mapR' f res
mapR _ _             = []

mapR' :: (a -> b) -> Resource a -> [b]
mapR' f (Resource r (Success x:xs)) = f x : mapR' f (Resource r xs)
mapR' _ (Resource r _)              = case r of () -> []

filterR :: (a -> Bool) -> IOResult (Resource a) -> [a]
filterR p (Success res) = filterR' p res
filterR _ _             = []

filterR' :: (a -> Bool) -> Resource a -> [a]
filterR' p (Resource r (Success x:xs))
 | p x       = x : filterR' p (Resource r xs)
 | otherwise = filterR' p (Resource r xs)
filterR' _ (Resource r _) = case r of () -> []

takeR :: Integral i => i -> IOResult (Resource a) -> [a]
takeR i (Success res) = takeR' i res
takeR _ _ = []

takeR' :: Integral i => i -> Resource a -> [a]
takeR' i (Resource r (Success x:xs)) | i > 0 = x : takeR' (i-1) (Resource r xs)
takeR' _ (Resource r _)                      = case r of () -> []

takeWhileR :: (a -> Bool) -> IOResult (Resource a) -> [a]
takeWhileR p (Success res) = takeWhileR' p res
takeWhileR _ _             = []

takeWhileR' :: (a -> Bool) -> Resource a -> [a]
takeWhileR' p (Resource r (Success x:xs)) | p x = x : takeWhileR' p (Resource r xs)
takeWhileR' _ _                                 = []

inFileResource :: FilePath -> IOResult (Resource String) :-> IOResult (Resource String)
inFileResource = iooi' . readFileIO

readFileIO :: FilePath -> IO (Resource String)
readFileIO fp = do
  { h <- lazy $ IO.openFile fp ReadMode
  ; r <- lazy $ IO.hClose h
  ; s <- lazy $ getlines r h
  ; return (Resource { release = r, stream = s })
  }
  where
    getlines r hdl = do 
      { el <- lazy $ try $ IO.hGetLine hdl
      ; case el of
          Left  e -> case r of () -> return [Failure $ show $ (e :: SomeException)]
          Right l -> do { ls <- lazy $ getlines r hdl
                        ; return (Success l:ls)
                        }
      }

outFileResource :: FilePath -> [String] -> IOResult (Resource ()) :-> IOResult (Resource ())
outFileResource = (iooi' .) . writeFileIO

writeFileIO :: FilePath -> [String] -> IO (Resource ())
writeFileIO fp lls = do
  { h <- lazy $ IO.openFile fp WriteMode
  ; r <- lazy $ IO.hClose h
  ; s <- lazy $ putlines r h lls
  ; return (Resource { release = r, stream = s })
  }
  where
    putlines r hdl []  = IO.hFlush hdl >> case r of () -> return []
    putlines r hdl (l:ls) = do 
      { eu <- lazy $ try $ IO.hPutStrLn hdl l
      ; case eu of
          Left  e -> case r of () -> return [Failure $ show $ (e :: SomeException)]
          Right u -> do { us <- lazy $ putlines r hdl ls
                        ; return (Success u:us)
                        }
      }

lazy :: IO a -> IO a
lazy = unsafeInterleaveIO
