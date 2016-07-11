{-# LANGUAGE OverloadedStrings #-}

-- | Microbenchmark of ByteString.hGetLine which uses Handle/IORefs

module Main where

import Control.Monad
import Data.Knob
-- import Criterion.Main
import Data.ByteString.Char8 as B
import System.IO as I

test01 :: Int -> IO ByteString
test01 n = do
  knob <- newKnob (pack [])
  h <- newFileHandle knob "test.txt" ReadWriteMode
  -- B.hPutStrLn h "Start writing."
  forM_ [1..n] $ \_ -> do
    B.hPutStrLn h "Hello world!"

  I.putStrLn "START GETS"

  forM_ [1..n] $ \_ -> do
    _l <- B.hGetLine h
    return ()

  I.putStrLn "DONE GETS"
    
  hClose h
  Data.Knob.getContents knob


main :: IO ()
main =
  do x <- test01 100
     print x

