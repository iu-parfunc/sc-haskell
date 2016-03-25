#!/usr/bin/env stack
-- stack --no-system-ghc --verbosity silent --resolver lts-3.8 --install-ghc runghc  --package turtle --package filemanip --package split

--   --package optparse-applicative

-- | This is a shell script to extract the latest tarball for each package.

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- module Main where

-- import qualified Control.Foldl as L
import           System.FilePath.Find as F
import           Turtle as T
import           Control.Monad
import           Prelude as P
import qualified Data.Map as M
import           Debug.Trace
import           Filesystem.Path.CurrentOS as S
import           Data.List.Split (splitOn)
import           Data.List as L
import           System.Posix.Files (createSymbolicLink, fileExist)
import           System.Directory (removeFile, doesFileExist)
import           Control.Exception (catch, SomeException, IOException)
import           System.Directory (getCurrentDirectory)

--------------------------------------------------------------------------------


chatter :: String -> IO ()
chatter s = putStrLn $ " [GrabNewestPerPackage.hs] " ++ s

main :: IO ()
main =
  do
     topDir <- fmap S.decodeString getCurrentDirectory

     let dataDir :: T.FilePath
         dataDir = topDir </> "data"
         inputDir = dataDir </> "0_hackage_all_tarballs/"
         outputDir = dataDir </> "1_only_newest_versions"

     chatter$ "finding all tarballs in input data set: "++(S.encodeString inputDir)
     ls <- F.find (return True) (F.extension ==? ".gz") (S.encodeString inputDir)

     let mp :: M.Map String S.FilePath
         mp = foldl buildMap M.empty (ls :: [P.FilePath])
         buildMap m0 pth =
           let pth' = S.decodeString pth
               (root,ver) = getVersion pth'
           in
           case M.lookup root m0 of
             Nothing -> M.insert root pth' m0
             Just pth2 -> if ver > (snd (getVersion pth2))
                          then M.insert root pth' m0
                          else m0

     chatter$ "Found "++show (M.size mp)++" distinct packages, among "++show (length ls)++" tarballs."

     chatter$ "Ensuring output dir exists..."
     T.mktree outputDir

     chatter "Symlinking newest versions of each tarball..."
     forM_ (M.toList mp) $ \(_root,path) ->
       do
          let -- (file: "0_hackage_all_tarballs" : _rest) = reverse $ S.splitDirectories path
              (file:olddir:_)   = reverse $ S.splitDirectories path
              newPath           = outputDir </> file
              newPath_s         = S.encodeString newPath
              linkTarget        = S.encodeString $ dataDir </> olddir </> file

          clearOutputFile newPath_s
          chatter $ "Creating link: " ++ newPath_s
          createSymbolicLink linkTarget newPath_s
          return ()
     chatter "Done."


-- | Checking for existence of broken links is annoying.  This is a workaround.
clearOutputFile :: P.FilePath -> IO ()
clearOutputFile path =
   -- Returns false for broken symlink:
   -- e <- doesFileExist path
  catch (do removeFile path
            chatter $ "Removed existing file: "++path)
        (\(e::IOException) -> return ())


-- | Hacky, but necessary, because the versions look like extensions.
dropKnownExtensions :: S.FilePath -> S.FilePath
dropKnownExtensions pth
 | S.extension pth == Just "gz"  = dropKnownExtensions (S.dropExtension pth)
 | S.extension pth == Just "tar" = dropKnownExtensions (S.dropExtension pth)
 | otherwise = pth

type Version = [Int]

getVersion :: S.FilePath -> (String,Version)
getVersion pth' = (root,ver)
  where
   file = dropKnownExtensions $ S.filename pth'
   (ver0:rest) = reverse $ splitOn "-" $ S.encodeString file
   root = L.concat $ L.intersperse "-" $ reverse rest
   ver = parseVersion ver0

parseVersion :: String -> Version
parseVersion str = map read $ splitOn "." str
