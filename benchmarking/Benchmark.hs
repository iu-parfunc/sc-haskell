module Main (main) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (mapMaybe)
import Data.Version (makeVersion)

import Distribution.Compat.ReadP
import Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName(..))
import Distribution.PackageDescription.Parse (ParseResult(..), parsePackageDescription)
import Distribution.Text (Text(..), simpleParse)
import Distribution.Version (isSpecificVersion)

import Network.HTTP.Client

import System.FilePath ((</>), (<.>))

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

downloadCabalFile :: Manager -> PackageIdentifier -> IO ByteString
downloadCabalFile m pkgId = do
    let pkgIdStr = show $ disp pkgId
        pkgStr   = show $ disp $ case pkgId of PackageIdentifier pkg _ -> pkg
    initialRequest <- parseUrl $ "GET http://hackage.haskell.org/package"
                              </> pkgIdStr </> pkgStr <.> "cabal"
    responseBody <$> httpLbs initialRequest m

main :: IO ()
main = do
    {-
    cnf <- readFile $! "test.config"
    let vers = case simpleParse cnf :: Maybe CabalConfig of
          Nothing -> error "Parse error"
          Just (CabalConfig cc) -> mapMaybe toPackageIdentifier cc
    -}
    manager   <- newManager defaultManagerSettings
    cabalFile <- downloadCabalFile manager (PackageIdentifier (PackageName "hello") (makeVersion [1,0,0,2]))
    let pkgDescr = case parsePackageDescription (L.unpack cabalFile) of
                     ParseFailed pe -> error $ show pe
                     ParseOk _ d    -> d
    print pkgDescr
