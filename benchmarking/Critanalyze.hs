#!/usr/bin/env stack
{- stack
    --no-system-ghc
    --resolver lts-5.16
    --install-ghc runghc
    --package bytestring
    --package cassava
    --package containers
    --package directory
    --package filepath
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Critanalyze where

import           Control.Monad

import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import           Data.Foldable (Foldable(..))
import           Data.Function
import           Data.List
import qualified Data.Set as Set (difference, intersection, fromList)
import           Data.Set (Set)

import           System.Directory
import           System.Environment
import           System.Exit (die)
import           System.FilePath
import           System.IO

data CSVRow = CSVRow {
    rowName     ::                !String
  , rowMean     :: {-# UNPACK #-} !Double
  , rowMeanLB   :: {-# UNPACK #-} !Double
  , rowMeanUB   :: {-# UNPACK #-} !Double
  , rowStddev   :: {-# UNPACK #-} !Double
  , rowStddevLB :: {-# UNPACK #-} !Double
  , rowStddevUB :: {-# UNPACK #-} !Double
  } deriving (Eq, Ord, Read, Show)

instance FromNamedRecord CSVRow where
    parseNamedRecord r = CSVRow <$> r .: "Name"
                                <*> r .: "Mean"
                                <*> r .: "MeanLB"
                                <*> r .: "MeanUB"
                                <*> r .: "Stddev"
                                <*> r .: "StddevLB"
                                <*> r .: "StddevUB"

analyze :: FilePath -> FilePath -> IO ()
analyze dir1 dir2 = do
    let accrueCsvs :: FilePath -> IO (Set FilePath)
        accrueCsvs fp = Set.fromList .  filter ((==) ".csv" . takeExtension)
                                    <$> listDirectory fp

        warnOrphanFiles :: FilePath -> Set FilePath -> IO ()
        warnOrphanFiles dir fps = unless (null fps) $ do
            hPutStrLn stderr $ "warning: The following files in " ++ dir ++ " have no counterpart!"
            forM_ fps $ \orp -> putStr ('\t':orp)
            putStrLn ""

        warnOrphanRows :: FilePath -> [CSVRow] -> IO ()
        warnOrphanRows csv crls = unless (null crls) $ do
            hPutStrLn stderr $ "warning: The following rows in " ++ csv ++ " have no counterpart!"
            forM_ crls $ putStr . ('\t':) . rowName
            putStrLn ""

        decodeCsv :: FilePath -> IO [CSVRow]
        decodeCsv fp = do
            csvData <- BL.readFile fp
            pure $ case decodeByName csvData of
                 Left msg     -> error msg
                 Right (_, v) -> toList v

    csvs1 <- accrueCsvs dir1
    csvs2 <- accrueCsvs dir2
    let csvs         = Set.intersection csvs1 csvs2
        csvs1Orphans = Set.difference   csvs1 csvs
        csvs2Orphans = Set.difference   csvs2 csvs
    warnOrphanFiles dir1 csvs1Orphans
    warnOrphanFiles dir2 csvs2Orphans

    forM_ csvs $ \csv -> do
        let csv1 = dir1 </> csv
            csv2 = dir2 </> csv
        csvRows1 <- decodeCsv csv1
        csvRows2 <- decodeCsv csv2
        let csvRows         = intersectBy ((==) `on` rowName) csvRows1 csvRows2
            csvRows1Orphans = diffBy ((==) `on` rowName) csvRows1 csvRows
            csvRows2Orphans = diffBy ((==) `on` rowName) csvRows2 csvRows
        warnOrphanRows csv1 csvRows1Orphans
        warnOrphanRows csv2 csvRows2Orphans

main :: IO ()
main = do
    args <- getArgs
    case args of
         dir1:dir2:_ -> do
             exitIfFakeDir dir1
             exitIfFakeDir dir2
             analyze dir1 dir2
         _ -> putStrLn usage

exitIfFakeDir :: FilePath -> IO ()
exitIfFakeDir dir = do
    exists <- doesDirectoryExist dir
    unless exists $ die $ "error: No such directory: " ++ dir

usage :: String
usage = "./Critanalyze <directory1> <directory2>"

diffBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
diffBy = foldl' . flip . deleteBy

listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (filter f) <$> (getDirectoryContents path)
  where f filename = filename /= "." && filename /= ".."
