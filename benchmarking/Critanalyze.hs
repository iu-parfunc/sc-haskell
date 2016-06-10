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
import           Data.List (nub, sortOn)
import           Data.Monoid ((<>))
import qualified Data.Vector as V
import           Data.Vector (Vector)

import           GHC.Generics

import           System.Directory
import           System.Environment
import           System.Exit (die)
import           System.FilePath
import           System.IO

import           Text.Printf

data CSVRow = CSVRow {
    crProgname :: !String
  , crCPUTime  :: {-# UNPACK #-} !Double
  , crRSqrTime :: {-# UNPACK #-} !Double
  } deriving (Eq, Generic, Ord, Read, Show)
instance NFData CSVRow

instance FromNamedRecord CSVRow where
    parseNamedRecord r = CSVRow <$> r .: "PROGNAME"
                                <*> r .: "CPUTIME"
                                <*> r .: "RSQR_TIME_FIT"

data CritanalyzeDiff
  = CDFirst  !CSVRow
  | CDSecond !CSVRow
  | CDBoth                !String -- Name
           {-# UNPACK #-} !Double -- CPU time 1
           {-# UNPACK #-} !Double -- R^2 time 1
           {-# UNPACK #-} !Double -- CPU time 2
           {-# UNPACK #-} !Double -- R^2 time 2
           {-# UNPACK #-} !Double -- Ratio of CPU time 2 to CPU time 1
  deriving (Eq, Generic, Ord, Read, Show)
instance NFData CritanalyzeDiff

diffToCritanalyzeDiff :: Diff CSVRow -> CritanalyzeDiff
diffToCritanalyzeDiff (First  cr) = CDFirst  cr
diffToCritanalyzeDiff (Second cr) = CDSecond cr
diffToCritanalyzeDiff (Both CSVRow{ crProgname = crpn
                                  , crCPUTime  = crct1
                                  , crRSqrTime = crrst1 }
                            CSVRow{ crCPUTime  = crct2
                                  , crRSqrTime = crrst2 })
    = CDBoth crpn crct1 crrst1 crct2 crrst2 crmpc
  where
    crmpc :: Double
    crmpc = crct2 / crct1

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
    go (CDFirst  cr)        = crProgname cr
    go (CDSecond cr)        = crProgname cr
    go (CDBoth n _ _ _ _ _) = n
rbRowNames _ = mempty

accrueCsvs :: FilePath -> IO [FilePath]
accrueCsvs fp = sortOn takeFileName . filter ((== ".csv") . takeExtension) <$> getDirectoryContentsRecursive fp

warnOrphans :: Foldable f => String -> (a -> String) -> FilePath -> f a -> IO ()
warnOrphans nouns name dir fps = unless (null fps) $ do
    hPutStrLn stderr $ "warning: The following "
                     ++ nouns ++ " in " ++ dir
                     ++ " have no counterpart!"
    forM_ fps $ hPutStrLn stderr . ('\t':) . name

warnOrphanFiles :: FilePath -> [FilePath] -> IO ()
warnOrphanFiles = warnOrphans "files" takeFileName

warnOrphanRows :: FilePath -> [CSVRow] -> IO ()
warnOrphanRows = warnOrphans "rows" crProgname

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
doubleFormatWidth = max 16 noResultLen

percentDecimals :: Int
percentDecimals = 1

percentWidth :: Int
percentWidth = max 10 noResultLen

roundToPlace :: Double -> Int -> Double
roundToPlace x n = (fromInteger . round $ x * (10^n)) / (10.0^^n)

analyze :: FilePath -> FilePath -> IO ()
analyze dir1 dir2 = do
    csvs1 <- force <$> accrueCsvs dir1
    csvs2 <- force <$> accrueCsvs dir2
    let csvsDiff             = getDiffBy ((==) `on` takeFileName) csvs1 csvs2
        (firsts, _, seconds) = partitionDiff csvsDiff
    warnOrphanFiles dir1 firsts
    warnOrphanFiles dir2 seconds

    rb <- fmap V.fromList . forM csvsDiff $ \case
        First  fp -> RBFirst  (dropExtension $ takeFileName fp)
                   . V.fromList <$> decodeCsv (dir1 </> fp)
        Second fp -> RBSecond (dropExtension $ takeFileName fp)
                  . V.fromList <$> decodeCsv (dir2 </> fp)
        Both fp1 fp2 -> do
            let fp1' = dir1 </> fp1
                fp2' = dir2 </> fp2
            csvRows1 <- decodeCsv fp1'
            csvRows2 <- decodeCsv fp2'
            let csvRowsDiff = getDiffBy ((==) `on` crProgname) csvRows1 csvRows2
                (firstRows, _, secondRows) = partitionDiff csvRowsDiff
                csvRowsCDiff = V.fromList $ map diffToCritanalyzeDiff csvRowsDiff
            warnOrphanRows fp1' firstRows
            warnOrphanRows fp2' secondRows
            pure $ RBBoth (dropExtension $ takeFileName fp1) csvRowsCDiff

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

    putStrLn "Critanalyze results (OLS CPU time)\n"
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
        printFirst CSVRow{crProgname = r, crCPUTime = m}
            = printRow fmtRBNameS r
                       fmtColumnE m
                       fmtColumnS noResult
                       fmtChangeS noResult
        printSecond CSVRow{crProgname = r, crCPUTime = m}
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
                CDBoth r m1 _ m2 _ pc -> do
                    let f = roundToPlace (percent pc) 1
                    printRow fmtRBNameS r
                             fmtColumnE m1
                             fmtColumnE m2
                             (fmtChangeF f) f

    let ratios = [ pc | RBBoth _ cdiffs <- rb
                      , CDBoth _ _ rs1 _ rs2 pc <- cdiffs
                      , rs1 >= 0.95 && rs2 >= 0.95
                      ]
        minRatio     = minimum ratios
        maxRatio     = maximum ratios

        -- Adapted from nofib-analyse
        sqr x     = x * x
        numRatios = fromIntegral (length ratios)
        logs      = fmap log ratios
        lbar      = sum logs / numRatios
        st_devs   = fmap (sqr . (lbar-)) logs
        dbar      = sum st_devs / numRatios
        geoMean   = exp lbar
        sdf       = exp (sqrt dbar)

    putStrLn banner
    printSummary "Min"            minRatio
    printSummary "Max"            maxRatio
    printSummary "-1 s.d."        (geoMean / sdf)
    printSummary "+1 s.d."        (geoMean * sdf)
    printSummary "Geometric mean" geoMean

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

-- Taken from tar
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir0 =
  fmap tail (recurseDirectories dir0 [""])

recurseDirectories :: FilePath -> [FilePath] -> IO [FilePath]
recurseDirectories _    []         = return []
recurseDirectories base (dir:dirs) = do
  (files, dirs') <- collect [] [] =<< getDirectoryContents (base </> dir)

  files' <- recurseDirectories base (dirs' ++ dirs)
  return (dir : files ++ files')

  where
    collect files dirs' []              = return (reverse files, reverse dirs')
    collect files dirs' (entry:entries) | ignore entry
                                        = collect files dirs' entries
    collect files dirs' (entry:entries) = do
      let dirEntry  = dir </> entry
          dirEntry' = addTrailingPathSeparator dirEntry
      isDirectory <- doesDirectoryExist (base </> dirEntry)
      if isDirectory
        then collect files (dirEntry':dirs') entries
        else collect (dirEntry:files) dirs' entries

    ignore ['.']      = True
    ignore ['.', '.'] = True
    ignore _          = False
