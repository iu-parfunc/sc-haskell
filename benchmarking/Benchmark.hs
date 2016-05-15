#!/usr/bin/env stack
{- stack
    --no-system-ghc
    --resolver lts-5.13
    --install-ghc runghc
    --package aeson
    --package async
    --package bytestring
    --package Cabal
    --package conduit
    --package conduit-extra
    --package deepseq
    --package directory
    --package filepath
    --package http-client
    --package http-client-tls
    --package process
    --package resourcet
    --package tar
    --package text
    --package time
    --package transformers
    --package unordered-containers
    --package witherable
    --package yaml
    --package zlib
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import           Control.Concurrent.Async
import           Control.DeepSeq (($!!))
import           Control.Exception (bracket)
import           Control.Monad
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Resource (runResourceT)

import           Data.Aeson.Types (Value(..), (.=), object)
import           Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process
import           Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format
import qualified Data.Vector as V
import           Data.Witherable (Witherable(..), forMaybe)
import           Data.Yaml (decodeFile, encodeFile)

import           Distribution.Compat.ReadP
import           Distribution.Package (Dependency(..), PackageIdentifier(..))
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse (ParseResult(..), parsePackageDescription)
import           Distribution.Text (Text(..), simpleParse)
import           Distribution.Version (isSpecificVersion)

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           System.Directory
import           System.Environment
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>), (<.>), takeFileName)
import           System.IO

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
    pure ()

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
benchBuildDirPrefix = ".bench-build"

benchResDirPrefix :: FilePath
benchResDirPrefix = ".bench-res"

baseStackageURL :: String
baseStackageURL = "https://www.stackage.org"

stackageTarget :: Target
stackageTarget = TargetLts 5 16

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

downloadStackageCabalConfig :: Manager -> FilePath -> IO ()
downloadStackageCabalConfig m cabalConfigPath = do
    initialRequest <- parseUrl $ "GET " ++ stackageCabalConfigURL
    bs <- withProgress ("Downloading " ++ stackageCabalConfigURL) $
        responseBody <$> httpLbs initialRequest m
    BL.writeFile cabalConfigPath bs

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

getPkgsWithBenchmarks :: Manager
                      -> FilePath
                      -> [PackageIdentifier]
                      -> IO [String]
getPkgsWithBenchmarks m benchBuildDir pkgIds = do
    -- let stackYamlFile = benchBuildDir </> "stack" <.> "yaml"
        -- TODO: Remove hack
    let cacheFile     = benchBuildDir </> ".awful-hacky-cache"
    -- TODO: Remove hack
    exists <- doesFileExist cacheFile -- stackYamlFile
    if exists
       then do
         -- TODO: Remove hack
         stackYaml <- decodeFile cacheFile -- stackYamlFile
         pure $ case stackYaml of
              Just (Object (HM.lookup "packages" -> Just (Array packages))) ->
                    map (\(String s) -> takeFileName $ T.unpack s)
                        (V.toList packages)
              -- TODO: Remove hack
              _ -> error $ "Corrupt " ++ cacheFile {-stackYamlFile-} ++ " file."

       else do
         pkgIdStrs <- forMaybe pkgIds $ \pkgId -> do
             cabalFile <- downloadCabalFile m pkgId
             let pkgIdStr = show $ disp pkgId
                 pkgDescr = case parsePackageDescription (BL.unpack cabalFile) of
                              ParseFailed pe -> error $ show pe
                              ParseOk _ d    -> d
             let benches = condBenchmarks pkgDescr
             if null benches
                then pure Nothing
                else do
                  putStrLn $ pkgIdStr ++ " has benchmarks, they are:"
                  for_ benches $ \p -> putStrLn ('\t':fst p)

                  let pkgBuildDir = benchBuildDir </> pkgIdStr
                  dirExists <- withProgressYesNo ("Checking if " ++ pkgBuildDir ++ " exists") $
                      doesDirectoryExist pkgBuildDir
                  unless dirExists $
                      extractPkgTarball m benchBuildDir pkgIdStr

                  pure (Just pkgIdStr)

         -- TODO: Remove hack
         writeCacheFile cacheFile pkgIdStrs
         -- writeStackDotYaml stackYamlFile pkgIdStrs
         pure pkgIdStrs

writeStackDotYaml :: FilePath
                  -> [String]
                  -> IO ()
writeStackDotYaml fileLoc _pkgIdStrs =
    let -- packages = "packages" .= map ("." </>) pkgIdStrs
        resolver = "resolver" .= targetSlug stackageTarget
        yaml     = object [resolver{-, packages-}]
    in encodeFile fileLoc yaml

-- TODO: This is a temporary hack. Remove when Stackage vets benchmarks.
writeCacheFile :: FilePath
               -> [String]
               -> IO ()
writeCacheFile fileLoc pkgIdStrs =
    let packages = "packages" .= pkgIdStrs
    in encodeFile fileLoc (object [packages])

type ShellM = ExceptT (String, Int) IO

{-
invoke :: FilePath -> [String] -> ShellM ()
invoke = invokeCommon $ \_ cproc -> do
    (ClosedStream, Inherited, Inherited, procH) <- streamingProcess cproc
    waitForStreamingProcess procH
-}

invokeTee :: FilePath -> FilePath -> [String] -> ShellM ()
invokeTee teeFile = invokeCommon $ \fullCmd cproc -> withFile teeFile AppendMode $ \teeH -> do
    hPutStrLn teeH $ "+ " ++ fullCmd
    (ClosedStream, out, err, procH) <- streamingProcess cproc
    let sink = CL.mapM_ $ liftIO . BS.putStr
    runConcurrently $
           Concurrently (runResourceT $ out $$ conduitHandle teeH =$ sink)
        *> Concurrently (runResourceT $ err $$ conduitHandle teeH =$ sink)
        *> Concurrently (waitForStreamingProcess procH)

invokeCommon :: (String -> CreateProcess -> IO ExitCode) -> FilePath -> [String] -> ShellM ()
invokeCommon action cmd args = do
    let fullCmd    = unwords (cmd:args)
        createProc = proc cmd args
    ec <- liftIO $ do
        putStrLn $ "+ " ++ fullCmd
        action fullCmd createProc
    case ec of
         ExitSuccess   -> pure ()
         ExitFailure c -> throwError (fullCmd, c)

runBenchmarks :: FilePath -> ShellM ()
runBenchmarks benchResPrefix = do
    -- TODO REALLY IMPORTANT: Remove hacky-stack.yaml hack
    let stackYamlFile                  = "hacky-stack" <.> "yaml"
        teeFile                        = benchResPrefix <.> "log"
        invokeWithYamlFile subcmd args = invokeTee teeFile "stack" $ subcmd:["--stack-yaml", stackYamlFile] ++ args
    liftIO $ do
        exists <- doesFileExist stackYamlFile
        unless exists $ writeStackDotYaml stackYamlFile []

    invokeWithYamlFile "setup" []
    -- TODO: Determine a way to run individual benchmarks
    invokeWithYamlFile "bench" ["--only-dependencies"]
    -- TODO: Timeout after, say, 10 minutes
    invokeWithYamlFile "bench"
        [ "--benchmark-arguments=" ++ unwords
            [ "--output", benchResPrefix <.> "html"
            , "--csv",    benchResPrefix <.> "csv"
            , "--raw",    benchResPrefix <.> "crit"
            ]
        ]

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
    manager <- newManager tlsManagerSettings

    let targetStr     = targetSlug stackageTarget
        benchBuildDir = benchBuildDirPrefix </> targetStr
    createDirectoryIfMissing True benchBuildDir

    let cabalConfigFile = benchBuildDir </> "cabal" <.> "config"
    exists <- doesFileExist cabalConfigFile
    unless exists $ downloadStackageCabalConfig manager cabalConfigFile

    cnf <- readFile cabalConfigFile
    let pkgIds = case simpleParse $!! cnf :: Maybe CabalConfig of
          Nothing -> error "Parse error"
          Just (CabalConfig cc) -> mapMaybe toPackageIdentifier cc

    pkgIdStrs <- getPkgsWithBenchmarks manager benchBuildDir pkgIds
    putStrLn "-------------------------"
    putStrLn "Packages with benchmarks:"
    for_ pkgIdStrs $ \pkgIdStr -> putStrLn ('\t':pkgIdStr)

    t <- getCurrentTime
    buildNum <- fromMaybe "numUnknown"  <$> lookupEnv "BUILD_NUMBER"
    nodeName <- fromMaybe "nodeUnknown" <$> lookupEnv "NODE_NAME"
    let ft          = formatTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S" t
        benchResDir = benchResDirPrefix </> targetStr

    for_ pkgIdStrs $ \pkgIdStr -> do
        let pkgBuildDir = benchBuildDir </> pkgIdStr
            pkgResDir   = benchResDir
                      </> ("jenkinsbuild-" ++ buildNum)
                      </> ("node-" ++ nodeName)
                      </> ft
        createDirectoryIfMissing True pkgResDir
        pkgResDir' <- canonicalizePath pkgResDir
        putStrLn "-------------------------"
        putStrLn $ "Benchmarking " ++ pkgIdStr
        putStrLn $ "Entering " ++ pkgBuildDir
        let benchResPrefix = pkgResDir' </> pkgIdStr
            benchResLog    = benchResPrefix <.> "log"
        res <- withCurrentDirectory pkgBuildDir
                 $ runExceptT
                 $ runBenchmarks benchResPrefix
        case res of
             Left (cmd, c) -> do
                 let errMsg = "ERROR: " ++ cmd ++ " returned exit code " ++ show c
                 putStrLn errMsg
                 appendFile benchResLog errMsg
             Right () -> pure ()
