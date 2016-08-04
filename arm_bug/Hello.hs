{-# LANGUAGE BangPatterns #-}

import Data.IORef
import Control.Concurrent

data Foo = Foo Int deriving Show

{-# NOINLINE mkfoo #-}
mkfoo x = Foo x

{-# NOINLINE dowrite #-}
dowrite r n = writeIORef r $! mkfoo n

main = 
  do r <- newIORef $! (Foo 3)
     forkIO (dowrite r 4)
     x <- readIORef r
     print x
