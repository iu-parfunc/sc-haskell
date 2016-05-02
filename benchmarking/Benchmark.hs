#!/usr/bin/env stack
{- stack
    --no-system-ghc
    --resolver lts-5.13
    --install-ghc runghc
    --package bytestring
    --package Cabal
    --package directory
    --package filepath
    --package http-client
    --package tar
    --package zlib
-}

module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import           Control.Monad (unless)

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Foldable (for_)
import           Data.Maybe (mapMaybe)
import           Data.Version (makeVersion)

import           Distribution.Compat.ReadP
import           Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(..))
import           Distribution.PackageDescription (GenericPackageDescription(..), PackageDescription(..))
import           Distribution.PackageDescription.Parse (ParseResult(..), parsePackageDescription)
import           Distribution.Text (Text(..), simpleParse)
import           Distribution.Version (isSpecificVersion)

import           Network.HTTP.Client

import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>), (<.>))

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

withProgress :: String -> IO a -> IO a
withProgress progressStr action = do
    putStr $ progressStr ++ "... "
    res <- action
    putStrLn "Done!"
    pure res

downloadCabalFile :: Manager -> PackageIdentifier -> IO ByteString
downloadCabalFile m pkgId = do
    let pkgIdStr = show $ disp pkgId
        pkgStr   = show $ disp $ case pkgId of PackageIdentifier pkg _ -> pkg
        cabalURL = baseHackageURL </> pkgIdStr </> pkgStr <.> "cabal"
    initialRequest <- parseUrl $ "GET " ++ cabalURL
    withProgress ("Downloading " ++ cabalURL) $
        responseBody <$> httpLbs initialRequest m

extractPkgTarball :: Manager -> PackageIdentifier -> IO ()
extractPkgTarball m pkgId = do
    let pkgIdStr    = show $ disp pkgId
        cabalTarURL = baseHackageURL </> pkgIdStr </> pkgIdStr <.> "tar" <.> "gz"
    initialRequest <- parseUrl $ "GET " ++ cabalTarURL
    tarBytesCompressed <- withProgress ("Downloading " ++ cabalTarURL) $
        responseBody <$> httpLbs initialRequest m
    withProgress ("Unpacking to " ++ benchBuildDir </> pkgIdStr) $ do
        let tarBytesDecompressed = GZip.decompress tarBytesCompressed
            tarball              = Tar.read tarBytesDecompressed
        Tar.unpack benchBuildDir tarball

main :: IO ()
main = do
    cnf <- readFile $! "test.config"
    let vers = case simpleParse cnf :: Maybe CabalConfig of
          Nothing -> error "Parse error"
          Just (CabalConfig cc) -> mapMaybe toPackageIdentifier cc
    manager <- newManager defaultManagerSettings
    for_ vers $ \ver -> do
        cabalFile <- downloadCabalFile manager ver
        let pkgDescr = case parsePackageDescription (L.unpack cabalFile) of
                         ParseFailed pe -> error $ show pe
                         ParseOk _ d    -> d
        putStr $ show $ disp $ package $ packageDescription pkgDescr
        putStrLn " benchmarks: "
        let benches = condBenchmarks pkgDescr
        for_ benches $ \p -> putStrLn $ '\t':fst p ++ " "
        unless (null benches) $
            extractPkgTarball manager ver
