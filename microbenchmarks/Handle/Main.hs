{-# LANGUAGE OverloadedStrings #-}

-- | Microbenchmark of ByteString.hGetLine which uses Handle/IORefs

module Main where

import Control.Monad
import Data.Knob
-- import Criterion.Main
import Data.ByteString.Char8 as B
import System.IO as I

import System.Posix.Temp (mkstemp)

test01 :: Int -> IO ByteString
test01 n = do
  knob <- newKnob (pack [])
  h <- newFileHandle knob "test.txt" ReadWriteMode
  -- B.hPutStrLn h "Start writing."
  forM_ [1..n] $ \_ -> do
    B.hPutStrLn h "Hello world!"

  I.putStrLn "START GETS"
  I.hSeek h AbsoluteSeek 0 
  forM_ [1..n] $ \_ -> do
    _l <- B.hGetLine h
    return ()

  I.putStrLn "DONE GETS"
    
  hClose h
  Data.Knob.getContents knob


test02 :: Int -> IO ()
test02 n = do
  knob <- newKnob (pack [])
  (f,h) <- mkstemp "test.txt" 
  forM_ [1..n] $ \_ -> do
    B.hPutStrLn h "hi"
  hClose h

  h2 <- openFile f ReadMode
  I.putStrLn "START GETS"
  forM_ [1..n] $ \_ -> do
    _l <- B.hGetLine h2
    return ()
  hClose h2
  I.putStrLn "DONE GETS"



main :: IO ()
main =
  do I.putStrLn "Test01"
     x <- test01 100
     print x

     I.putStrLn "Test02"
     test02 100

