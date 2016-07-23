-- |

{-# LANGUAGE Safe #-}

module Main where

import Control.Monad.ST
import Data.STRef
import Control.Concurrent

main :: IO ()
main = do
  x <- stToIO $ newSTRef (0 :: Int)
  y <- stToIO $ newSTRef (0 :: Int)
  forkIO $ do stToIO       $ writeSTRef x 1
              y' <- stToIO $ readSTRef y
              putStrLn $ "T2: x = 1, y = " ++ show y'
  stToIO       $ writeSTRef y 1
  x' <- stToIO $ readSTRef x
  putStrLn $ "T1: y = 1, x = " ++ show x'
