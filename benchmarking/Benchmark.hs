#!/usr/bin/env stack
{- stack
    --no-system-ghc
    --resolver lts-5.13
    --install-ghc runghc
    --package aeson
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
import           System.Process

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

benchBuildDirPrefix :: FilePath
benchBuildDirPrefix = "bench-build-"

baseStackageURL :: String
baseStackageURL = "https://www.stackage.org"

stackageTarget :: Target
stackageTarget = TargetLts 5 13

stackageCabalConfigURL :: String
stackageCabalConfigURL =  baseStackageURL
                      </> targetSlug stackageTarget
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

extractPkgTarball :: Manager -> FilePath -> String -> IO ()
extractPkgTarball m benchBuildDir pkgIdStr = do
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
        yaml = object ["resolver" .= targetSlug stackageTarget]
    in encodeFile path yaml

type ShellM = ExceptT (String, Int) IO

invoke :: FilePath -> [String] -> ShellM ()
invoke cmd args = do
    let fullCmd = unwords (cmd:args)
    ec <- liftIO $ do
        let createProc = proc cmd args
        (_, _, _, handle) <- createProcess createProc
        ec' <- waitForProcess handle
        pure ec'
    case ec of
         ExitSuccess   -> pure ()
         ExitFailure c -> throwError (fullCmd, c)

runBenchmarks :: ShellM ()
runBenchmarks = do
    liftIO $ do
        let yamlFile = "stack.yaml"
        exists <- doesFileExist yamlFile
        unless exists $ writeStackDotYaml yamlFile
    invoke "stack" ["setup"]
    -- TODO: Determine a way to run individual benchmarks
    invoke "stack" ["bench"]

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
            let pkgIdStr      = show $ disp ver
                benchBuildDir = benchBuildDirPrefix ++ targetSlug stackageTarget </> pkgIdStr
                pkgBuildDir   = benchBuildDir </> pkgIdStr
            dirExists <- withProgressYesNo ("Checking if " ++ pkgBuildDir ++ " exists") $
                doesDirectoryExist pkgBuildDir
            unless dirExists $
                extractPkgTarball manager benchBuildDir pkgIdStr
            res <- withCurrentDirectory pkgBuildDir $ runExceptT runBenchmarks
            case res of
                 Left (cmd, c) -> do
                     putStrLn $ "ERROR: " ++ cmd ++ " returned exit code " ++ show c
                 Right () -> putStrLn "It worked!"
