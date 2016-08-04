{-# LANGUAGE BangPatterns #-}

import Data.IORef
import Control.Concurrent

data Foo = Foo Int deriving Show

{-# NOINLINE mkfoo #-}
mkfoo x = Foo x

{-# NOINLINE dowrite #-}
dowrite r = writeIORef r $! mkfoo 4

main = 
  do r <- newIORef $! (Foo 3)
     forkIO (dowrite r)
     x <- readIORef r
     print x
