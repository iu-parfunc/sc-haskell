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
    --package statistics
    --package vector
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.DeepSeq
import           Control.Monad

import           Data.Algorithm.Diff
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Csv
import           Data.Foldable (Foldable(..))
import           Data.Function
import           Data.List (nub, sort)
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import           Data.Vector (Vector)

import           GHC.Generics

import           Statistics.Sample (geometricMean)

import           System.Directory
import           System.Environment
import           System.Exit (die)
import           System.FilePath
import           System.IO

import           Text.Printf

data CSVRow = CSVRow {
     crName     ::                !String
  ,  crMean     :: {-# UNPACK #-} !Double
  , _crMeanLB   :: {-# UNPACK #-} !Double
  , _crMeanUB   :: {-# UNPACK #-} !Double
  , _crStddev   :: {-# UNPACK #-} !Double
  , _crStddevLB :: {-# UNPACK #-} !Double
  , _crStddevUB :: {-# UNPACK #-} !Double
  } deriving (Eq, Generic, Ord, Read, Show)
instance NFData CSVRow

instance FromNamedRecord CSVRow where
    parseNamedRecord r = CSVRow <$> r .: "Name"
                                <*> r .: "Mean"
                                <*> r .: "MeanLB"
                                <*> r .: "MeanUB"
                                <*> r .: "Stddev"
                                <*> r .: "StddevLB"
                                <*> r .: "StddevUB"

data CritanalyzeDiff
  = CDFirst  !CSVRow
  | CDSecond !CSVRow
  | CDBoth                !String -- Name
           {-# UNPACK #-} !Double -- Mean 1
           {-# UNPACK #-} !Double -- Mean 2
           {-# UNPACK #-} !Double -- Ratio of mean 2 to mean 1
  deriving (Eq, Generic, Ord, Read, Show)
instance NFData CritanalyzeDiff

diffToCritanalyzeDiff :: Diff CSVRow -> CritanalyzeDiff
diffToCritanalyzeDiff (First  cr) = CDFirst  cr
diffToCritanalyzeDiff (Second cr) = CDSecond cr
diffToCritanalyzeDiff (Both CSVRow{crName = crn, crMean = crm1}
                            CSVRow{crMean = crm2}) = CDBoth crn crm1 crm2 crmpc
  where
    crmpc :: Double
    crmpc = crm2 / crm1

data ReportBlock
  = RBFirst
      !String          -- Name
      !(Vector CSVRow) -- Rows

   | RBSecond
      !String          -- Name
      !(Vector CSVRow) -- Rows

   | RBBoth
      !String                   -- Name
      !(Vector CritanalyzeDiff) -- Row diffs

  deriving (Eq, Generic, Ord, Read, Show)
instance NFData ReportBlock

rbName :: ReportBlock -> String
rbName (RBFirst  n _) = n
rbName (RBSecond n _) = n
rbName (RBBoth   n _) = n

rbRowNames :: ReportBlock -> Vector String
rbRowNames (RBBoth _ rows) = fmap go rows
  where
    go :: CritanalyzeDiff -> String
    go (CDFirst  cr)    = crName cr
    go (CDSecond cr)    = crName cr
    go (CDBoth n _ _ _) = n
rbRowNames _ = mempty

accrueCsvs :: FilePath -> IO [FilePath]
accrueCsvs fp = sort . filter ((== ".csv") . takeExtension) <$> listDirectory fp

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
    csvData <- BL.unpack <$> BL.readFile fp
    -- HACK ALERT: Sometimes, criterion writes into its .csv files multiple
    -- headers of the form:
    --
    --   Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB
    --
    -- cassava only checks for this once at the beginning, so if it pops up again
    -- at a later point in the file, parsing will fail. To avoid this pitall, we
    -- remove duplicate lines from the file. This should only nab lines that look
    -- like the above, since every other row is preceded with a unique label.
    let csvData' = BL.pack . unlines . nub . lines $ csvData
    pure $ case decodeByName $!! csvData' of
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
doubleFormatWidth = max 16 noResultLen

percentDecimals :: Int
percentDecimals = 1

percentWidth :: Int
percentWidth = max 10 noResultLen

roundToPlace :: Double -> Int -> Double
roundToPlace x n = (fromInteger . round $ x * (10^n)) / (10.0^^n)

analyze :: FilePath -> FilePath -> IO ()
analyze dir1 dir2 = do
    csvs1 <- accrueCsvs dir1
    csvs2 <- accrueCsvs dir2
    let csvsDiff             = getDiff csvs1 csvs2
        (firsts, _, seconds) = partitionDiff csvsDiff
    warnOrphanFiles dir1 firsts
    warnOrphanFiles dir2 seconds

    rb <- fmap V.fromList . forM csvsDiff $ \case
        First  fp -> RBFirst  (dropExtension fp)
                   . V.fromList <$> decodeCsv (dir1 </> fp)
        Second fp -> RBSecond (dropExtension fp)
                  . V.fromList <$> decodeCsv (dir2 </> fp)
        Both _ fp -> do
            let fp1 = dir1 </> fp
                fp2 = dir2 </> fp
            csvRows1 <- decodeCsv fp1
            csvRows2 <- decodeCsv fp2
            let csvRowsDiff = getDiffBy ((==) `on` crName) csvRows1 csvRows2
                (firstRows, _, secondRows) = partitionDiff csvRowsDiff
                csvRowsCDiff = V.fromList $ map diffToCritanalyzeDiff csvRowsDiff
            warnOrphanRows fp1 firstRows
            warnOrphanRows fp2 secondRows
            pure $ RBBoth (dropExtension fp) csvRowsCDiff

    let rbNameLens        = fmap (length . rbName) rb
        rbRowNameLens     = fmap ((+ padding) . length) $ V.concatMap rbRowNames rb
        maxRBNameLen      = maximum $ V.cons benchNameLen rbNameLens <> rbRowNameLens
        changeColumnWidth = max changeLen percentWidth

        bannerPad = replicate padding '-'
        banner    = replicate maxRBNameLen      '-' ++ bannerPad
                 ++ replicate doubleFormatWidth '-' ++ bannerPad
                 ++ replicate doubleFormatWidth '-' ++ bannerPad
                 ++ replicate changeColumnWidth '-' ++ bannerPad

        pad        = replicate padding ' '
        fmtRBNameS = "%" ++ show maxRBNameLen ++ "s"
        fmtColumn  = "%" ++ show doubleFormatWidth
        fmtColumnS = fmtColumn ++ "s"
        fmtColumnE = fmtColumn ++ '.':show doubleDecimals ++ "e"
        fmtChangeS = "%"  ++ show changeColumnWidth ++ "s"
        fmtChangeF f = "%" ++ (if f > 0.0 then "+" else "")
                            ++ show (changeColumnWidth - 1)
                            ++ '.':show percentDecimals ++ "f%%"
        percent pc = (pc - 1.0) * 100

    putStrLn "Critanalyze results (mean)\n"
    putStrLn banner
    printf   fmtRBNameS benchName
    putStr   pad
    printf   fmtColumnS $ take doubleFormatWidth dir1
    putStr   pad
    printf   fmtColumnS $ take doubleFormatWidth dir2
    putStr   pad
    printf   fmtChangeS change
    putStrLn pad
    putStrLn banner

    let printRow :: (PrintfArg r, PrintfArg m1, PrintfArg m2, PrintfArg pc)
                 => String -> r
                 -> String -> m1
                 -> String -> m2
                 -> String -> pc
                 -> IO ()
        printRow fmtR r fmtM1 m1 fmtM2 m2 fmtPC pc = do
            printf   fmtR  r
            putStr   pad
            printf   fmtM1 m1
            putStr   pad
            printf   fmtM2 m2
            putStr   pad
            printf   fmtPC pc
            putStrLn pad

        printFirst, printSecond :: CSVRow -> IO ()
        printFirst CSVRow{crName = r, crMean = m}
            = printRow fmtRBNameS r
                       fmtColumnE m
                       fmtColumnS noResult
                       fmtChangeS noResult
        printSecond CSVRow{crName = r, crMean = m}
            = printRow fmtRBNameS r
                       fmtColumnS noResult
                       fmtColumnE m
                       fmtChangeS noResult

        printSummary :: String -> Double -> IO ()
        printSummary n pc =
            let f = roundToPlace (percent pc) 1
            in printRow fmtRBNameS n
                        fmtColumnS ("-----" :: String)
                        fmtColumnS ("-----" :: String)
                        (fmtChangeF f) f

    forM_ rb $ \case
        RBFirst n csvs -> do
            putStrLn n
            forM_ csvs printFirst
        RBSecond n csvs -> do
            putStrLn n
            forM_ csvs printSecond
        RBBoth n cdiffs -> do
            putStrLn n
            forM_ cdiffs $ \case
                CDFirst  cr -> printFirst  cr
                CDSecond cr -> printSecond cr
                CDBoth r m1 m2 pc -> do
                    let f = roundToPlace (percent pc) 1
                    printRow fmtRBNameS r
                             fmtColumnE m1
                             fmtColumnE m2
                             (fmtChangeF f) f

    let ratios = [ pc | RBBoth _ cdiffs <- rb
                      , CDBoth _ _ _ pc <- cdiffs ]
        minRatio     = minimum ratios
        maxRatio     = maximum ratios
        geoMeanRatio = geometricMean ratios

    putStrLn banner
    printSummary "Min"            minRatio
    printSummary "Max"            maxRatio
    printSummary "Geometric mean" geoMeanRatio

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
