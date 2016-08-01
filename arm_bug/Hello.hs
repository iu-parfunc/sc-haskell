{-# LANGUAGE BangPatterns #-}

import Data.IORef
import Control.Concurrent

data Foo = Foo Int
  deriving Show

{- 
{-# NOINLINE mul #-}
mul :: Foo -> Foo -> Int
mul (Foo x) (Foo y) = x * y
-}

{-# NOINLINE mkfoo #-}
mkfoo x = 
  Foo x

main = 
  do print "hi"
     r <- newIORef $! (Foo 3)
     forkIO $ writeIORef r $! mkfoo 4
     x <- readIORef r
     print x
--     print (mul x x)
