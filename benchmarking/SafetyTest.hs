#!/usr/bin/env stack
{- stack
    --no-system-ghc
    --resolver lts-5.16
    --install-ghc runghc
    --package aeson
    --package async
    --package bytestring
    --package Cabal
    --package clock
    --package conduit
    --package conduit-extra
    --package deepseq
    --package directory
    --package extra
    --package filepath
    --package formatting
    --package http-client
    --package http-client-tls
    --package optparse-generic
    --package process
    --package resourcet
    --package safe
    --package tar
    --package text
    --package time
    --package transformers
    --package unordered-containers
    --package witherable
    --package yaml
    --package zlib
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import           Control.Concurrent.Async
import           Control.DeepSeq (($!!))
import           Control.Exception.Extra (bracket, catch, retry)
import           Control.Monad.Extra
import           Control.Monad.Except (MonadError(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Resource (runResourceT)

import           Data.Aeson.Types (Value(..), (.=), object, toJSON)
import           Data.Bool (bool)
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process
import           Data.Either (partitionEithers)
import           Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import           Data.List.Extra (chunksOf)
import           Data.Maybe (fromMaybe, isJust)
import           Data.Monoid ((<>))
import qualified Data.Text    as TS
import qualified Data.Text.IO as TS
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format
import           Data.Traversable (for)
import qualified Data.Vector as V
import           Data.Witherable (Witherable(..), forMaybe)
import           Data.Yaml (array, decodeFile, encodeFile)

import           Distribution.Compat.ReadP
import           Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(..))
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse (ParseResult(..), parsePackageDescription)
import           Distribution.Text (Text(..), simpleParse)
import           Distribution.Version (isSpecificVersion)

import           Formatting (sformat)
import           Formatting.Clock (timeSpecs)

import           GHC.Generics (Generic)

import           Options.Generic (ParseRecord, getRecord)

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

import           Safe (atDef)

import           System.Clock
import           System.Directory.Extra
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>), (<.>), takeFileName, takeDirectory)
import           System.IO

-----
-- Taken from stackage-curator

data Target = TargetNightly !Day
            | TargetLts !Int !Int
    deriving (Eq, Ord, Read, Show)

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
benchBuildDirPrefix = "bench-build"

benchResDirPrefix :: FilePath
benchResDirPrefix = "bench-res"

baseStackageURL :: String
baseStackageURL = "https://www.stackage.org"

stackageTarget :: Target
stackageTarget = TargetLts 5 16

stackageCabalConfigURL :: String
stackageCabalConfigURL =  baseStackageURL
                      </> targetSlug stackageTarget
                      </> "cabal" <.> "config"

hsbencherURL :: String
hsbencherURL = "https://github.com/iu-parfunc/HSBencher/archive"
           </> hsbencherSHA <.> "tar" <.> "gz"

hsbencherSHA :: String
hsbencherSHA = "1b699fc6fda4e0ec40c668f2841fd7da6c18abe5"

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

httpRetry :: Request -> Manager -> IO (Response ByteString)
httpRetry r = retry 10 . httpLbs r

downloadStackageCabalConfig :: Manager -> FilePath -> IO ()
downloadStackageCabalConfig m cabalConfigPath = do
    initialRequest <- parseUrl $ "GET " ++ stackageCabalConfigURL
    bs <- withProgress ("Downloading " ++ stackageCabalConfigURL) $
        responseBody <$> httpRetry initialRequest m
    BL.writeFile cabalConfigPath bs

downloadCabalFile :: Manager -> PackageIdentifier -> IO ByteString
downloadCabalFile m pkgId = do
    let pkgIdStr = show $ disp pkgId
        pkgStr   = show $ disp $ case pkgId of PackageIdentifier pkg _ -> pkg
        cabalURL = baseHackageURL </> pkgIdStr </> pkgStr <.> "cabal"
    initialRequest <- parseUrl $ "GET " ++ cabalURL
    withProgress ("Downloading " ++ cabalURL) $
        responseBody <$> httpRetry initialRequest m

extractPkgTarball :: Manager -> FilePath -> String -> IO ()
extractPkgTarball m benchBuildDir pkgIdStr = do
    let cabalTarURL = baseHackageURL </> pkgIdStr </> pkgIdStr <.> "tar" <.> "gz"
    initialRequest <- parseUrl $ "GET " ++ cabalTarURL
    let loop = do
          tarBytesCompressed <- withProgress ("Downloading " ++ cabalTarURL) $
            responseBody <$> httpRetry initialRequest m
          withProgress ("Unpacking to " ++ benchBuildDir </> pkgIdStr) $ do
            let tarBytesDecompressed = GZip.decompress tarBytesCompressed
                tarball              = Tar.read tarBytesDecompressed
            Tar.unpack benchBuildDir tarball `catch` \(e :: Tar.FormatError) -> loop
    loop

getPkgsWithBenchmarks :: Manager
                      -> FilePath
                      -> [PackageIdentifier]
                      -> IO [String]
getPkgsWithBenchmarks m benchBuildDir pkgIds = do
    -- let stackYamlFile = benchBuildDir </> "stack" <.> "yaml"
        -- TODO: Remove hack
    let ts            = targetSlug stackageTarget
        cacheFileName = ".cached-" ++ ts
        cacheFile1    = (takeDirectory . takeDirectory) benchBuildDir </> cacheFileName
    -- TODO: Remove hack
    exists1 <- doesFileExist cacheFile1 -- stackYamlFile
    let cacheFile = if exists1
                      then cacheFile1
                      else benchBuildDir </> cacheFileName
    exists <- doesFileExist cacheFile
    if exists
       then do
         -- TODO: Remove hack
         stackYaml <- decodeFile cacheFile -- stackYamlFile
         pure $ case stackYaml of
              Just (Object (HM.lookup "packages" -> Just (Array packages))) ->
                    map (\(String s) -> takeFileName $ TS.unpack s)
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
                  pure (Just pkgIdStr)

         -- TODO: Remove hack
         writeCacheFile cacheFile pkgIdStrs
         -- writeStackDotYaml stackYamlFile pkgIdStrs
         pure pkgIdStrs

downloadPkgTarballs :: Manager
                    -> FilePath
                    -> [String]
                    -> IO ()
downloadPkgTarballs m benchBuildDir pkgIdStrs = for_ pkgIdStrs $ \pkgIdStr -> do
    let pkgBuildDir = benchBuildDir </> pkgIdStr
    dirExists <- withProgressYesNo ("Checking if " ++ pkgBuildDir ++ " exists") $
        doesDirectoryExist pkgBuildDir
    unless dirExists $ extractPkgTarball m benchBuildDir pkgIdStr

writeStackDotYaml :: Maybe String
                  -> FilePath
                  -> FilePath
                  -> [String]
                  -> IO ()
writeStackDotYaml dockerfile mountDir fileLoc _pkgIdStrs =
    let -- packages = "packages" .= map ("." </>) pkgIdStrs

        skipGhcCheck = "skip-ghc-check" .= True

        ts       = targetSlug stackageTarget
        resolver = "resolver" .= ts
        packages = "packages" .= array
                     [ toJSON ("." :: String)
                     , object
                         [ "location" .= object
                             [ "git"    .= ("https://github.com/bos/criterion" :: String)
                             , "commit" .= ("fa26f39a187f422adbb513ea459ee2700301f804" :: String)
                             ]
                         , "extra-dep" .= True
                         ]
                     ]
        extraDeps = "extra-deps" .= array [toJSON ("criterion-1.1.1.0" :: String)]
        -- Invariant: only used if the dockerfile argument is non-Nothing
        docker df = "docker" .= object
                     [ "enable"    .= True
                     , "repo"      .= df
                     , "auto-pull" .= True
                     , "set-user"  .= True
                     , "mount"     .= [mountDir]
                     ]
        yaml = object $ [ resolver
                        , packages
                        , extraDeps
                        , skipGhcCheck
                        ]
                        ++
                        (case dockerfile of
                           Just df -> [docker df]
                           Nothing -> [])
    in encodeFile fileLoc yaml

-- TODO: This is a temporary hack. Remove when Stackage vets benchmarks.
writeCacheFile :: FilePath
               -> [String]
               -> IO ()
writeCacheFile fileLoc pkgIdStrs =
    let packages = "packages" .= pkgIdStrs
    in encodeFile fileLoc (object [packages])

type ShellM = ExceptT (String, Int) IO

invoke :: FilePath -> [String] -> ShellM ()
invoke = invokeCommon $ \_ cproc -> do
    (ClosedStream, Inherited, Inherited, procH) <- streamingProcess cproc
    waitForStreamingProcess procH

invokeTee :: FilePath -> FilePath -> [String] -> ShellM ()
invokeTee teeFile = invokeCommon $ \fullCmd cproc -> withFile teeFile AppendMode $ \teeH -> do
    hPutStrLn teeH $ "+ " ++ fullCmd
    startTime <- getTime Monotonic
    (ClosedStream, out, err, procH) <- streamingProcess cproc
    let sink = CL.mapM_ $ liftIO . BS.putStr
    ec <- runConcurrently $
           Concurrently (runResourceT $ out $$ conduitHandle teeH =$ sink)
        *> Concurrently (runResourceT $ err $$ conduitHandle teeH =$ sink)
        *> Concurrently (waitForStreamingProcess procH)
    endTime <- getTime Monotonic
    let duration = sformat timeSpecs startTime endTime
    for_ [stdout, teeH] $ \h -> TS.hPutStrLn h ("Total benchmarking time: " <> duration)
    pure ec

invokeCommon :: (String -> CreateProcess -> IO ExitCode) -> FilePath -> [String] -> ShellM ()
invokeCommon action cmd args = do
    let fullCmd    = unwords (cmd:args)
        createProc = shell fullCmd
    ec <- liftIO $ do
        putStrLn $ "+ " ++ fullCmd
        action fullCmd createProc
    case ec of
         ExitSuccess   -> pure ()
         ExitFailure c -> throwError (fullCmd, c)

runBenchmarks :: FilePath -- hsbencher-fusion-upload-criterion
              -> String -- Package + version
              -> Maybe String
              -> FilePath
              -> FilePath
              -> ShellM ()
runBenchmarks hfuc pkgIdStr dockerfile mountDir benchResPrefix = do
    -- TODO REALLY IMPORTANT: Remove hacky-stack.yaml hack
    let stackYamlFile                  = "hacky-stack" <.> "yaml"
        teeFile                        = benchResPrefix <.> "log"
        invokeWithYamlFile subcmd args = invokeTee teeFile "stack" $ subcmd:["--stack-yaml", stackYamlFile] ++ args
        reportFiles = map (benchResPrefix <.>)
                        [ "html"
                        , "csv"
                        , "crit"
                        , "json"
                        -- , "eventlog"
                        ]
    liftIO $ do
        writeStackDotYaml dockerfile mountDir stackYamlFile []
        for_ reportFiles $ \f -> do
            whenM (doesFileExist f) $ removeFile f

    invokeWithYamlFile "setup" []
    -- TODO: Determine a way to run individual benchmarks
    invokeWithYamlFile "bench" ["--only-dependencies"]
{-
    -- TODO: Timeout after, say, 10 minutes of inactivity
    invokeWithYamlFile "bench"
        [ "--ghc-options='-rtsopts'" -- -eventlog
        , "--benchmark-arguments='" ++ unwords
            [ "+RTS", "-T", "-RTS" -- -lu-s-g-p
            , "--output="  ++ benchResPrefix <.> "html"
            -- , "--csv="     ++ benchResPrefix <.> "csv"
            -- , "--raw="     ++ benchResPrefix <.> "crit"
            , "--json="    ++ benchResPrefix <.> "json"
            , "--regress=" ++ "allocated:iters"
            , "--regress=" ++ "bytesCopied:iters"
            , "--regress=" ++ "cycles:iters"
            , "--regress=" ++ "numGcs:iters"
            , "--regress=" ++ "mutatorWallSeconds:iters"
            , "--regress=" ++ "gcWallSeconds:iters"
            , "--regress=" ++ "cpuTime:iters"
            -- , "--iters=1"

            -- Try to run for longer to reduce noise
            , "-L", "20"
            ] ++ "'"
        ]
-}

{-
    invoke "mkdir" ["-p", benchResPrefix]
    invoke "cp" ["*.eventlog", benchResPrefix]
-}

    let resolver = targetSlug stackageTarget
        pkgId  = case simpleParse pkgIdStr of
                      Just p  -> p
                      Nothing -> error $ "Ill-formatted PackageIdentifier: " ++ pkgIdStr
    invoke hfuc $ [ "--noupload"
                  , "--csv=" ++ benchResPrefix <.> "csv"
                  , "--custom=PACKAGE,"    ++ unPackageName (pkgName pkgId)
                  , "--custom=PACKAGEVER," ++ show (disp $ pkgVersion pkgId)
                  , "--custom=RESOLVER,"   ++ resolver
                  ]
                  ++
                  (case dockerfile of
                     Just df -> ["--variant=" ++ df]
                     Nothing -> [])
                  ++
                  [ "--json"
                  , benchResPrefix <.> "json"
                  ]

installHSBencherFusion :: Manager -> FilePath -> IO FilePath
installHSBencherFusion m unpkDir = do
    let hsbencherFileName    = "HSBencher-" ++ hsbencherSHA
        hsbencherTarballName = hsbencherFileName <.> "tar" <.> "gz"
        hsbencherDir         = unpkDir </> hsbencherFileName
    unlessM (doesDirectoryExist hsbencherDir) $ do
      initialRequest <- parseUrl $ "GET " ++ hsbencherURL
      withProgress ("Downloading " ++ hsbencherURL) $ do
          bytes <- httpRetry initialRequest m
          for_ bytes $ BL.writeFile hsbencherTarballName
      putStrLn "Proceeding to install HSBencher"
      res <- runExceptT $ do
          invoke "tar" ["-xvf", hsbencherTarballName]
          res' <- liftIO $ withCurrentDirectory hsbencherDir $ do
            runExceptT $ invoke "stack" ["install", "hsbencher-fusion"]
          ExceptT $ pure res'
      case res of
           Left (cmd, c) ->
             let errMsg = "ERROR: " ++ cmd ++ " returned exit code " ++ show c
             in error errMsg
           Right () -> pure ()
    pure $ hsbencherDir </> "bin" </> "hsbencher-fusion-upload-criterion"

data CmdArgs = CmdArgs
    { target     :: Maybe String -- TODO: This could perhaps be formatted better
    , dockerfile :: Maybe String
    , slice      :: Maybe Int -- TODO: Require slice and numSlices be given together
    , numSlices  :: Maybe Int
    } deriving (Eq, Ord, Read, Show, Generic, ParseRecord)

main :: IO ()
main = do
    cmdArgs <- getRecord "Stackage benchmarking script"
    doIt cmdArgs

doIt :: CmdArgs -> IO ()
doIt cmdArgs = do
    let myRatio :: Maybe (Int, Int)
        myRatio = case (slice cmdArgs, numSlices cmdArgs) of
          (Just s, Just ns) -> Just (s, ns)
          _                 -> Nothing

    manager <- newManager tlsManagerSettings
    curDir  <- getCurrentDirectory

    hfuc <- installHSBencherFusion manager curDir

    let targetStr     = targetSlug stackageTarget
        benchBuildDir = benchBuildDirPrefix </> targetStr
    createDirectoryIfMissing True benchBuildDir

    let cabalConfigFile = benchBuildDir </> "cabal" <.> "config"
    unlessM (doesFileExist cabalConfigFile) $
        downloadStackageCabalConfig manager cabalConfigFile

    cnf <- readFile cabalConfigFile
    let pkgIds = case simpleParse $!! cnf :: Maybe CabalConfig of
          Nothing -> error "Parse error"
          Just (CabalConfig cc) -> mapMaybe toPackageIdentifier cc

    pkgIdStrs' <- getPkgsWithBenchmarks manager benchBuildDir pkgIds
    let numPkgs' = length pkgIdStrs'
        pkgIdStrs = case myRatio of
          Just (s, ns) -> let numPkgs'' = fromIntegral numPkgs'
                              ns'       = fromIntegral ns
                              chunkSize = ceiling (numPkgs'' / ns')
                              chunks    = chunksOf chunkSize pkgIdStrs'
                          in atDef [] chunks (s-1)
          Nothing -> pkgIdStrs'

--     downloadPkgTarballs manager benchBuildDir pkgIdStrs

    let numPkgsStr  = show $ length pkgIdStrs
        numPkgsStr' = show numPkgs'
    putStrLn "-------------------------"
    putStrLn $ numPkgsStr' ++ " packages with benchmarks"
    for_ pkgIdStrs' $ putStrLn . ('\t':)
    whenJust myRatio $ \(s, ns) -> do
        putStrLn $ "Given slice " ++ show s ++ " of " ++ show ns
                ++ " slices of packages, consisting of " ++ numPkgsStr ++ " packages:"
        for_ pkgIdStrs $ putStrLn . ('\t':)

    t <- getCurrentTime
    let ft          = formatTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S" t
        ts          = targetSlug stackageTarget
        benchResDir = benchResDirPrefix </> fromMaybe "" (dockerfile cmdArgs) </> targetStr
        pkgResDir   = benchResDir </> ft
    createDirectoryIfMissing True pkgResDir
    pkgResDir' <- canonicalizePath pkgResDir
    putStrLn $ "Logging results in " ++ pkgResDir'

    void $ runExceptT $ invokeTee "install-everything.log" "stack" $
      ["install"]  ++ pkgIdStrs

{-
    results <- for pkgIdStrs $ \pkgIdStr -> do
        let pkgBuildDir = benchBuildDir </> pkgIdStr
        pkgBuildDir' <- canonicalizePath pkgBuildDir
        putStrLn "-------------------------"
        putStrLn $ "Benchmarking " ++ pkgIdStr
        putStrLn $ "Entering " ++ pkgBuildDir'
        let benchResPrefix = pkgResDir' </> pkgIdStr
            benchResLog    = benchResPrefix <.> "log"
            mountDir       = curDir
        res <- withCurrentDirectory pkgBuildDir
                 $ runExceptT
                 $ runBenchmarks hfuc pkgIdStr (dockerfile cmdArgs) mountDir benchResPrefix
        case res of
             Left (cmd, c) -> do
                 let errMsg = "ERROR: " ++ cmd ++ " returned exit code " ++ show c
                 putStrLn errMsg
                 appendFile benchResLog errMsg
                 pure $ Left pkgIdStr
             Right () -> pure $ Right pkgIdStr

    unless (null results) $ do
        let (failures, successes) = partitionEithers results
            numSuccessesStr = show $ length successes
            numFailuresStr  = show $ length failures
        putStrLn "-------------------------"
        putStrLn $ numSuccessesStr ++ "/" ++ numPkgsStr ++ " packages succeeded"
        for_ successes $ \success -> putStrLn ('\t':success)
        putStrLn $ numFailuresStr   ++ "/" ++ numPkgsStr ++ " packages failed"
        for_ failures  $ \failure -> putStrLn ('\t':failure)
-}
