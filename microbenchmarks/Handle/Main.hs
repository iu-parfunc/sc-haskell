{-# LANGUAGE OverloadedStrings #-}

-- | Microbenchmark of ByteString.hGetLine which uses Handle/IORefs

module Main where

import Control.Monad
import Data.Knob
import Criterion.Main
import Criterion.Types
import Data.ByteString.Char8 as B
import System.IO as I

import System.Posix.Temp (mkstemp)

test01 :: Int -> IO ByteString
test01 n = do
  knob <- newKnob (pack [])
  h <- newFileHandle knob "test.txt" ReadWriteMode
  -- B.hPutStrLn h "Start writing."
  forM_ [1..n] $ \_ -> do
    B.hPutStrLn h "hi"
  I.hSeek h AbsoluteSeek 0 
  forM_ [1..n] $ \_ -> do
    _l <- B.hGetLine h
    return ()
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
  forM_ [1..n] $ \_ -> do
    _l <- B.hGetLine h2
    return ()
  hClose h2


main :: IO ()
main = defaultMain
  [ bench "in-mem put-get lines"    $ Benchmarkable (void . test01 . fromIntegral)
  , bench "from-disk put-get lines" $ Benchmarkable (       test02 . fromIntegral)
  ]
