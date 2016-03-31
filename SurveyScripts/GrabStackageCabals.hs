#!/usr/bin/env stack
-- stack --no-system-ghc --verbosity silent --resolver lts-3.8 --install-ghc runghc  --package turtle --package filemanip --package split

--   --package optparse-applicative

-- | This is a shell script to extract the latest tarball for each package.

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- module Main where

-- import qualified Control.Foldl as L
import           System.FilePath.Find as F
import           Turtle as T
import           Control.Monad
import           Prelude as P
import qualified Data.Map as M
import           Debug.Trace
import           Filesystem.Path.CurrentOS as S
import           Data.List.Split (splitOn)
import           Data.List as L
import           System.Posix.Files (createSymbolicLink, fileExist)
import           System.Directory (removeFile, doesFileExist)
import           Control.Exception (catch, SomeException, IOException)
import           System.Directory (getCurrentDirectory, setCurrentDirectory)
import  System.Process

--------------------------------------------------------------------------------


chatter :: String -> IO ()
chatter s = putStrLn $ " [GrabStackageCabals.hs] " ++ s

main :: IO ()
main =
  do
     topDir <- fmap S.decodeString getCurrentDirectory

     let inputDir  = topDir </> "stackage/package-list-5.10.txt"
         outputDir = topDir </> "stackage/cabals-5.10/"

     lines <- lines <$> readFile (S.encodeString inputDir)
     chatter$ "Read "++(show (length lines))++" package names.  Here's a sample:"
     mapM_ (putStrLn . ("  " ++)) (take 10 lines)

     system $ "mkdir -p " ++ (S.encodeString outputDir)
     setCurrentDirectory (S.encodeString outputDir)

     forM_ lines $ \ package ->
       do let base = "https://hackage.haskell.org/package/"
              cmd  = "wget " ++ base ++ package ++ "/" ++ stripVersion package ++ ".cabal"
          putStrLn $ "Executing: "++cmd
          system cmd


type Version = [Int]

stripVersion :: String -> String
stripVersion pth = s
   where (s,_) = getVersion (S.decodeString pth)

getVersion :: S.FilePath -> (String,Version)
getVersion pth' = (root,ver)
  where
   file = S.filename pth'
   (ver0:rest) = reverse $ splitOn "-" $ S.encodeString file
   root = L.concat $ L.intersperse "-" $ reverse rest
   ver = parseVersion ver0

parseVersion :: String -> Version
parseVersion str = map read $ splitOn "." str
