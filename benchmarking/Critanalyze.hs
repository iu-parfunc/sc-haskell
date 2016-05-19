#!/usr/bin/env stack
{- stack
    --no-system-ghc
    --resolver lts-5.16
    --install-ghc runghc
    --package bytestring
    --package cassava
    --package containers
    --package deepseq
    --package Diff
    --package directory
    --package filepath
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.DeepSeq
import           Control.Monad

import           Data.Algorithm.Diff
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import           Data.Foldable (Foldable(..))
import           Data.Function

import           GHC.Generics

import           System.Directory
import           System.Environment
import           System.Exit (die)
import           System.FilePath
import           System.IO

import           Text.Printf

data CSVRow = CSVRow {
    crName     ::                !String
  , crMean     :: {-# UNPACK #-} !Double
--   , crMeanLB   :: {-# UNPACK #-} !Double
--   , crMeanUB   :: {-# UNPACK #-} !Double
--   , crStddev   :: {-# UNPACK #-} !Double
--   , crStddevLB :: {-# UNPACK #-} !Double
--   , crStddevUB :: {-# UNPACK #-} !Double
  } deriving (Eq, Generic, Ord, Read, Show)
instance NFData CSVRow

instance FromNamedRecord CSVRow where
    parseNamedRecord r = CSVRow <$> r .: "Name"
                                <*> r .: "Mean"
--                                 <*> r .: "MeanLB"
--                                 <*> r .: "MeanUB"
--                                 <*> r .: "Stddev"
--                                 <*> r .: "StddevLB"
--                                 <*> r .: "StddevUB"

data CritanalyzeDiff
  = CDFirst  !CSVRow
  | CDSecond !CSVRow
  | CDBoth                !String -- Name
           {-# UNPACK #-} !Double -- Mean 1
           {-# UNPACK #-} !Double -- Mean 2
           {-# UNPACK #-} !Double -- Percent change
  deriving (Eq, Generic, Ord, Read, Show)
instance NFData CritanalyzeDiff

diffToCritanalyzeDiff :: Diff CSVRow -> CritanalyzeDiff
diffToCritanalyzeDiff (First  cr) = CDFirst  cr
diffToCritanalyzeDiff (Second cr) = CDSecond cr
diffToCritanalyzeDiff (Both CSVRow { crName = crn, crMean = crm1 }
                            CSVRow { crMean = crm2 }) = CDBoth crn crm1 crm2 crmpc
  where
    crmpc :: Double
    crmpc = ((crm2 - crm1) / abs crm1) * 100

data ReportBlock
  = RBFirst
      !String   -- Name
      ![CSVRow] -- Rows

   | RBSecond
      !String   -- Name
      ![CSVRow] -- Rows

   | RBBoth
      !String            -- Name
      ![CritanalyzeDiff] -- Row diffs

  deriving (Eq, Generic, Ord, Read, Show)
instance NFData ReportBlock

rbName :: ReportBlock -> String
rbName (RBFirst  n _) = n
rbName (RBSecond n _) = n
rbName (RBBoth   n _) = n

rbRowNames :: ReportBlock -> [String]
rbRowNames (RBBoth _ rows) = map go rows
  where
    go :: CritanalyzeDiff -> String
    go (CDFirst  cr)    = crName cr
    go (CDSecond cr)    = crName cr
    go (CDBoth n _ _ _) = n
rbRowNames _ = []

accrueCsvs :: FilePath -> IO [FilePath]
accrueCsvs fp = filter ((== ".csv") . takeExtension) <$> listDirectory fp

warnOrphans :: Foldable f => String -> (a -> String) -> FilePath -> f a -> IO ()
warnOrphans nouns name dir fps = unless (null fps) $ do
    hPutStrLn stderr $ "warning: The following "
                     ++ nouns ++ " in " ++ dir
                     ++ " have no counterpart!"
    forM_ fps $ putStr . ('\t':) . name
    putStrLn "\n"

warnOrphanFiles :: FilePath -> [FilePath] -> IO ()
warnOrphanFiles = warnOrphans "files" id

warnOrphanRows :: FilePath -> [CSVRow] -> IO ()
warnOrphanRows = warnOrphans "rows" crName

decodeCsv :: FilePath -> IO [CSVRow]
decodeCsv fp = do
    csvData <- BL.readFile fp
    pure $ case decodeByName $!! csvData of
         Left msg     -> error msg
         Right (_, v) -> toList $!! v

partitionDiff :: [Diff a] -> ([a], [(a, a)], [a])
partitionDiff xs = foldr selectDiff ([], [], []) xs
{-# INLINE partitionDiff #-}

selectDiff :: Diff a -> ([a], [(a, a)], [a]) -> ([a], [(a, a)], [a])
selectDiff x ~(fs, bs, ss) = case x of
    First f    -> (f:fs,          bs,   ss)
    Both b1 b2 -> (  fs, (b1, b2):bs,   ss)
    Second s   -> (  fs,          bs, s:ss)

padding :: Int
padding = 2

benchName :: String
benchName = "Benchmark name"

benchNameLen :: Int
benchNameLen = length benchName

change :: String
change = "Change"

changeLen :: Int
changeLen = length change

noResult :: String
noResult = "(no result)"

noResultLen :: Int
noResultLen = length noResult

doubleDecimals :: Int
doubleDecimals = 3

doubleFormatWidth :: Int
doubleFormatWidth = max 10 noResultLen

percentDecimals :: Int
percentDecimals = 1

percentWidth :: Int
percentWidth = max 10 noResultLen

analyze :: FilePath -> FilePath -> IO ()
analyze dir1 dir2 = do
    csvs1 <- accrueCsvs dir1
    csvs2 <- accrueCsvs dir2
    let csvsDiff             = getDiff csvs1 csvs2
        (firsts, _, seconds) = partitionDiff csvsDiff
    warnOrphanFiles dir1 firsts
    warnOrphanFiles dir2 seconds

    rb <- forM csvsDiff $ \case
        First  fp -> RBFirst  fp <$> decodeCsv (dir1 </> fp)
        Second fp -> RBSecond fp <$> decodeCsv (dir2 </> fp)
        Both _ fp -> do
            let fp1 = dir1 </> fp
                fp2 = dir2 </> fp
            csvRows1 <- decodeCsv fp1
            csvRows2 <- decodeCsv fp2
            let csvRowsDiff = getDiffBy ((==) `on` crName) csvRows1 csvRows2
                (firstRows, _, secondRows) = partitionDiff csvRowsDiff
                csvRowsCDiff = map diffToCritanalyzeDiff csvRowsDiff
            warnOrphanRows fp1 firstRows
            warnOrphanRows fp2 secondRows
            pure $ RBBoth fp csvRowsCDiff

    let rbNameLens        = map (length . rbName) rb
        rbRowNameLens     = map ((+ padding) . length) $ concatMap rbRowNames rb
        maxRBNameLen      = maximum $ benchNameLen:rbNameLens ++ rbRowNameLens
        column1Width      = max (length dir1) doubleFormatWidth
        column2Width      = max (length dir2) doubleFormatWidth
        changeColumnWidth = max changeLen percentWidth

        bannerPad = replicate padding '-'
        banner    = replicate maxRBNameLen      '-' ++ bannerPad
                 ++ replicate column1Width      '-' ++ bannerPad
                 ++ replicate column2Width      '-' ++ bannerPad
                 ++ replicate changeColumnWidth '-' ++ bannerPad

        pad         = replicate padding ' '
        fmtRBNameS  = "%" ++ show maxRBNameLen ++ "s"
        fmtColumn1  = "%" ++ show column1Width
        fmtColumn1S = fmtColumn1 ++ "s"
        fmtColumn1E = fmtColumn1 ++ '.':show doubleDecimals ++ "e"
        fmtColumn2  = "%" ++ show column2Width
        fmtColumn2S = fmtColumn2 ++ "s"
        fmtColumn2E = fmtColumn2 ++ '.':show doubleDecimals ++ "e"
        fmtChangeS  = "%" ++ show changeColumnWidth ++ "s"
        fmtChangeF  = "%" ++ show (changeColumnWidth - 1)
                          ++ '.':show percentDecimals ++ "f%%"

    putStrLn "Critanalyze results (mean)\n"
    putStrLn banner
    printf   fmtRBNameS benchName
    putStr   pad
    printf   fmtColumn1S dir1
    putStr   pad
    printf   fmtColumn2S dir2
    putStr   pad
    printf   fmtChangeS change
    putStrLn pad
    putStrLn banner

--     let printRow :: IO ()
--         printRow = return ()

    forM_ rb $ \case
        RBFirst n csvs -> do
            putStrLn n
            forM_ csvs $ \(CSVRow r m) -> do
                printf   fmtRBNameS  r
                putStr   pad
                printf   fmtColumn1E m
                putStr   pad
                printf   fmtColumn2S noResult
                putStr   pad
                printf   fmtChangeS  noResult
                putStrLn pad
        RBSecond n csvs -> do
            putStrLn n
            forM_ csvs $ \(CSVRow r m) -> do
                printf   fmtRBNameS  r
                putStr   pad
                printf   fmtColumn1S noResult
                putStr   pad
                printf   fmtColumn2E m
                putStr   pad
                printf   fmtChangeS  noResult
                putStrLn pad
        RBBoth n cdiffs -> do
            putStrLn n
            forM_ cdiffs $ \case
                CDFirst (CSVRow r m) -> do
                    printf   fmtRBNameS  r
                    putStr   pad
                    printf   fmtColumn1E m
                    putStr   pad
                    printf   fmtColumn2S noResult
                    putStr   pad
                    printf   fmtChangeS  noResult
                    putStrLn pad
                CDSecond (CSVRow r m) -> do
                    printf   fmtRBNameS  r
                    putStr   pad
                    printf   fmtColumn1S noResult
                    putStr   pad
                    printf   fmtColumn2E m
                    putStr   pad
                    printf   fmtChangeS  noResult
                    putStrLn pad
                CDBoth r m1 m2 pc -> do
                    printf   fmtRBNameS  r
                    putStr   pad
                    printf   fmtColumn1E m1
                    putStr   pad
                    printf   fmtColumn2E m2
                    putStr   pad
                    printf   fmtChangeF  pc
                    putStrLn pad


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

listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (filter f) <$> (getDirectoryContents path)
  where f filename = filename /= "." && filename /= ".."
