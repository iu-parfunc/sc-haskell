#!/usr/bin/env stack
{- stack
    --no-system-ghc
    --resolver lts-5.13
    --install-ghc runghc
    --package Aeson
    --package bytestring
    --package Cabal
    --package directory
    --package filepath
    --package http-client
    --package process
    --package tar
    --package transformers
    --package yaml
    --package zlib
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import           Control.Exception (bracket)
import           Control.Monad (unless, when, void)
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Except (ExceptT, runExceptT)

import           Data.Aeson.Types (Value, (.=), object)
import           Data.Bool (bool)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Foldable (for_)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as TS
import           Data.Time.Calendar (Day(..))
import           Data.Yaml (encodeFile)

import           Distribution.Compat.ReadP
import           Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(..))
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse (ParseResult(..), parsePackageDescription)
import           Distribution.Text (Text(..), simpleParse)
import           Distribution.Version (isSpecificVersion)

import           Network.HTTP.Client

import           System.Directory
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>), (<.>))
import           System.Process (readProcessWithExitCode)

-----
-- Taken from stackage-curator

data Target = TargetNightly !Day
            | TargetLts !Int !Int
    deriving Show

targetSlug :: Target -> String
targetSlug (TargetNightly day) = "nightly-" ++ show day
targetSlug (TargetLts x y) = concat ["lts-", show x, ".", show y]

-----

comments :: ReadP r ()
comments = do
    _ <- string "--"
    _ <- manyTill (satisfy (const True)) (char '\n')
    return ()

parser :: ReadP r [Dependency]
parser = do
    skipMany comments
    _ <- string "constraints:"
    many1 (skipSpaces *> parse <* optional (char ','))

newtype CabalConfig = CabalConfig [Dependency]
  deriving (Eq, Read, Show)

instance Text CabalConfig where
  disp = undefined
  parse = CabalConfig <$> parser

toPackageIdentifier :: Dependency -> Maybe PackageIdentifier
toPackageIdentifier (Dependency p vr) =
  PackageIdentifier p <$> isSpecificVersion vr

baseHackageURL :: String
baseHackageURL = "http://hackage.haskell.org/package"

benchBuildDir :: FilePath
benchBuildDir = "bench-build"

baseStackageURL :: String
baseStackageURL = "https://www.stackage.org"

stackageLTS :: Target
stackageLTS = TargetLts 5 13

stackageCabalConfigURL :: String
stackageCabalConfigURL =  baseStackageURL
                      </> targetSlug stackageLTS
                      </> "cabal" <.> "config"

withProgress :: String -> IO a -> IO a
withProgress progressStr = withProgressFinish progressStr (const "Done!")

withProgressFinish :: String -> (a -> String) -> IO a -> IO a
withProgressFinish progressStr finishStr action = do
    putStr $ progressStr ++ "... "
    res <- action
    putStrLn $ finishStr res
    pure res

withProgressYesNo :: String -> IO Bool -> IO Bool
withProgressYesNo progressStr = withProgressFinish progressStr (bool "No" "Yes")

downloadCabalFile :: Manager -> PackageIdentifier -> IO ByteString
downloadCabalFile m pkgId = do
    let pkgIdStr = show $ disp pkgId
        pkgStr   = show $ disp $ case pkgId of PackageIdentifier pkg _ -> pkg
        cabalURL = baseHackageURL </> pkgIdStr </> pkgStr <.> "cabal"
    initialRequest <- parseUrl $ "GET " ++ cabalURL
    withProgress ("Downloading " ++ cabalURL) $
        responseBody <$> httpLbs initialRequest m

extractPkgTarball :: Manager -> String -> IO ()
extractPkgTarball m pkgIdStr = do
    let cabalTarURL = baseHackageURL </> pkgIdStr </> pkgIdStr <.> "tar" <.> "gz"
    initialRequest <- parseUrl $ "GET " ++ cabalTarURL
    tarBytesCompressed <- withProgress ("Downloading " ++ cabalTarURL) $
        responseBody <$> httpLbs initialRequest m
    withProgress ("Unpacking to " ++ benchBuildDir </> pkgIdStr) $ do
        let tarBytesDecompressed = GZip.decompress tarBytesCompressed
            tarball              = Tar.read tarBytesDecompressed
        Tar.unpack benchBuildDir tarball

writeStackDotYaml :: FilePath -> IO ()
writeStackDotYaml path =
    let yaml :: Value
        yaml = object ["resolver" .= targetSlug stackageLTS]
    in encodeFile path yaml

type ShellM = ExceptT (Int, String, String) IO

shell :: Bool -> FilePath -> [String] -> ShellM String
shell verbose cmd args = do
    let fullCmd = unwords (cmd:args)
    (ec, stdout, stderr) <- liftIO $ do
        when verbose $ putStrLn fullCmd
        res@(_, stdout', _) <- readProcessWithExitCode cmd args ""
        when verbose $ putStrLn stdout'
        pure res
    case ec of
         ExitSuccess   -> pure stdout
         ExitFailure c -> throwError (c, fullCmd, stderr)

runBenchmarks :: ShellM ()
runBenchmarks = do
    liftIO $ do
        let yamlFile = "stack.yaml"
        exists <- doesFileExist yamlFile
        unless exists $ writeStackDotYaml yamlFile
    void $ shell False "stack" ["setup"]
    -- TODO: Determine a way to run individual benchmarks
    void $ shell True  "stack" ["bench"]

-- | Run an 'IO' action with the given working directory and restore the
-- original working directory afterwards, even if the given action fails due
-- to an exception.
--
-- The operation may fail with the same exceptions as 'getCurrentDirectory'
-- and 'setCurrentDirectory'.
withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    !cnf <- readFile "test.config"
    let vers = case simpleParse cnf :: Maybe CabalConfig of
          Nothing -> error "Parse error"
          Just (CabalConfig cc) -> mapMaybe toPackageIdentifier cc
    for_ vers $ \ver -> do
        cabalFile <- downloadCabalFile manager ver
        let pkgDescr = case parsePackageDescription (L.unpack cabalFile) of
                         ParseFailed pe -> error $ show pe
                         ParseOk _ d    -> d
        putStr $ show $ disp $ package $ packageDescription pkgDescr
        putStrLn " benchmarks: "
        let benches = condBenchmarks pkgDescr
        for_ benches $ \p -> putStrLn $ '\t':fst p ++ " "
        unless (null benches) $ do
            let pkgIdStr = show $ disp ver
                tarDir   = benchBuildDir </> pkgIdStr
            tarDirExists <- withProgressYesNo ("Checking if " ++ tarDir ++ " exists") $
                doesDirectoryExist tarDir
            unless tarDirExists $
                extractPkgTarball manager pkgIdStr
            res <- withCurrentDirectory tarDir $ runExceptT runBenchmarks
            case res of
                 Left (c, cmd, stderr) -> do
                     putStrLn $ "ERROR: " ++ cmd ++ " returned exit code " ++ show c
                     putStrLn "----------------------------------------"
                     putStrLn stderr
                     putStrLn "----------------------------------------"
                 Right () -> putStrLn "It worked!"
