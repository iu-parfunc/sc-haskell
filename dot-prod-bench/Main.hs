{-# LANGUAGE CPP #-}

import Data.Array.MArray
import Data.Array.IO
import Data.Array.ST
import Data.IORef
import Data.STRef
import Control.Concurrent
import Control.Monad
import Control.Monad.ST
import Criterion.Main

#ifndef NUMELEMS
#define NUMELEMS 200000
#endif

testVec1 :: IO (IOUArray Int Double)
testVec1 = newListArray (1,NUMELEMS) [0..]

testVec2 :: IO (IOUArray Int Double)
testVec2 = newListArray (1,NUMELEMS) [1..]

testVec1ST :: ST s (STUArray s Int Double)
testVec1ST = newListArray (1,NUMELEMS) [0..]

testVec2ST :: ST s (STUArray s Int Double)
testVec2ST = newListArray (1,NUMELEMS) [1..]
  
vectorSumSeq :: IOUArray Int Double
             -> IOUArray Int Double
             -> IO (IOUArray Int Double)
vectorSumSeq arr brr = do
  ia <- getBounds arr
  crr <- newArray_ ia
  forM_ (range ia) $ \i ->
      do x <- readArray arr i
         y <- readArray brr i
         writeArray crr i (x + y)
  return crr

vectorSumSeqST :: STUArray s Int Double
               -> STUArray s Int Double
               -> ST s (STUArray s Int Double)
vectorSumSeqST arr brr = do
  ia <- getBounds arr
  crr <- newArray_ ia
  forM_ (range ia) $ \i ->
      do x <- readArray arr i
         y <- readArray brr i
         writeArray crr i (x + y)
  return crr
  
dotProdSeq :: IOUArray Int Double
           -> IOUArray Int Double
           -> IO Double
dotProdSeq arr brr = do
  crr <- vectorSumSeq arr brr
  r <- newIORef 0
  ic <- getBounds crr
  forM_ (range ic) $ \i -> do
      x <- readArray crr i
      modifyIORef' r (+ x)
  readIORef r

dotProdSeqST :: STUArray s Int Double
             -> STUArray s Int Double
             -> ST s Double
dotProdSeqST arr brr = do
  crr <- vectorSumSeqST arr brr
  r <- newSTRef 0
  ic <- getBounds crr
  forM_ (range ic) $ \i -> do
      x <- readArray crr i
      modifySTRef' r (+ x)
  readSTRef r


main :: IO ()
main = do
  a <- testVec1
  b <- testVec2
  aST <- stToIO testVec1ST
  bST <- stToIO testVec2ST
  defaultMain [
        bgroup "sum" [ bench "io" $ whnfIO (vectorSumSeq a b)
                     , bench "st" $ whnfIO $ stToIO (vectorSumSeqST aST bST)
                     ],
        bgroup "dot" [ bench "io" $ whnfIO (dotProdSeq a b)
                     , bench "st" $ whnfIO $ stToIO (dotProdSeqST aST bST)
                     ]
       ]


