{-# LANGUAGE OverloadedStrings #-}

-- | Microbenchmark of ByteString.hGetLine which uses Handle/IORefs

module Main where

import Control.Monad
import Data.Knob
import Criterion.Main
import Criterion.Types
import Data.ByteString.Char8 as B
import System.IO as I
import System.Directory (removeFile)
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


-- test02 :: FilePath -> Int -> IO ()
test02 :: Handle -> Int -> IO ()
test02 h n = do
  -- h <- openFile f ReadMode
  hSeek h AbsoluteSeek 0
  forM_ [1..n] $ \_ -> do
    B.hPutStrLn h "hi"
  -- hClose h
  -- h2 <- openFile f ReadMode
  let h2 = h
  hSeek h2 AbsoluteSeek 0
  forM_ [1..n] $ \_ -> do
    _l <- B.hGetLine h2
    return ()    
  -- hClose h2

main :: IO ()
main = do
  (f,h) <- mkstemp "/tmp/test.txt"
  -- hClose h
  defaultMain
   [ bench "in-mem/put-N_get-N_lines"    $ Benchmarkable (void . test01 . fromIntegral)
   , bench "from-disk/put-N_get-N_lines" $ Benchmarkable (       test02 h . fromIntegral)
   , bench "in-mem/put-1000_get-1000"    $ whnfIO $ void $ test01 1000
   , bench "from-disk/put-1000_get-1000" $ whnfIO $ test02 h 1000
   ]
  I.putStrLn $ "Removing temp file: "++ f
  removeFile f
