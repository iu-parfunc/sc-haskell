#!/usr/bin/env stack
-- stack --no-system-ghc --verbosity silent --resolver lts-3.8 --install-ghc runghc --package Ghostbuster --package filemanip --package concurrent-output --package ascii-progress

-- | This script compiles the generated connected component definitions
-- that were ghostbusted in the previous step by 'ConnectedDDefs'.
--
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           ConnectedDDefs               ( Stats )
import qualified ConnectedDDefs               as CC ( Stats(..) )

import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad                (unless)
import           Control.Monad.IO.Class
import           Control.Monad.Par.Class
import           Control.Monad.Par.IO
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BC
import qualified Data.ByteString.Lazy         as BL
import           Data.Csv                     as CSV
import           Data.Default
import           Data.List                    as L
import           Data.Vector                  ( Vector )
import qualified Data.Vector                  as V
import           GHC.Conc                     (getNumCapabilities)
import           GHC.Generics
import           System.Console.AsciiProgress ( newProgressBar, tick, pgRegion, pgTotal, pgFormat )
import           System.Console.Concurrent
import           System.Console.Regions
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.FilePath.GlobPattern
import           System.IO
import           System.IO.Unsafe             (unsafePerformIO)
import           System.Process
import           Text.Printf


helpMessage :: String
helpMessage = unlines $
  [ "CompileBusted version 0.1"
  , ""
  , "  Synopsis:"
  , "    CompileBusted [-p GlobPattern] ghostbust_data.csv [output directory]"
  , ""
  , "  Usage:"
  , "    This will read in the data from the provided 'ghostbust_data.csv'"
  , "    to locate ghostbusted files it will then attempt to compile. Only"
  , "    those filenames which match the optional glob pattern will be"
  , "    compiled."
  , ""
  , "    The basic glob pattern syntax is the same as for the Unix shell"
  , "    environment."
  , ""
  , "     - '*' matches up to a directory separator or end of string"
  , "     - '[range]' matches any character in range"
  , "     - '[!range]' matches any character _not_ in range"
  , ""
  , "    There are three extensions to the traditional glob syntax, taken"
  , "    from modern unix shells."
  , ""
  , "     - '\\' escapes a character that might otherwise have special"
  , "       meaning. For a literal '\\' character, use '\\\\'."
  , "     - '**' matches everything, including a directory separator"
  , "     - '(s1|s2|...)' matches any of the strings _s1_, _s2_, etc."
  , ""
  , "    Logs of standard input and output resulting from the compilation"
  , "    are written using into the output directory, using the same"
  , "    directory structure as the input files."
  , ""
  , "    To compile the files using multiple GHC instances, run the script"
  , "    using multiple threads: +RTS -N<number of threads> -RTS"
  , ""
  ]

data Options = Options
  { globPattern         :: Maybe GlobPattern
  , inputCSV            :: FilePath
  , outputDir           :: FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { globPattern         = Nothing
  , inputCSV            = error helpMessage
  , outputDir           = "."
  }

data Result = Result
  { filePath            :: FilePath
  , degenerateCompiles  :: !Bool
  , variants            :: !Integer
  , successes           :: !Integer
  }
  deriving (Show, Generic, NFData)

instance CSV.FromNamedRecord Result
instance CSV.ToNamedRecord   Result
instance CSV.ToRecord        Result
instance CSV.DefaultOrdered  Result

instance CSV.ToField Bool where
  toField True  = "True"
  toField False = "False"

instance CSV.FromField Bool where
  parseField "True"  = return True
  parseField "False" = return False
  parseField x       = error $ printf "FromField: failed to parse %s :: Bool" (BC.unpack x)


-- Poor man's concurrent file IO. This is okay for us because we expect to
-- spend most of the time compiling files, rather than writing to the log.
--
data SharedHandle = SharedHandle
  { sHandle     :: Handle
  , sLock       :: MVar ()
  }

newSharedHandle :: IO Handle -> IO SharedHandle
newSharedHandle makeHandle =
  SharedHandle <$> makeHandle <*> newMVar ()

withSharedHandle :: SharedHandle -> (Handle -> IO a) -> IO a
withSharedHandle SharedHandle{..} f =
  withMVar sLock $ \() -> f sHandle

closeSharedHandle :: SharedHandle -> IO ()
closeSharedHandle SharedHandle{..} = do
  () <- takeMVar sLock
  hClose sHandle

withSharedFile :: FilePath -> IOMode -> (SharedHandle -> IO a) -> IO a
withSharedFile name mode =
  bracket (newSharedHandle (openFile name mode))
          closeSharedHandle


-- Attempt to compile ghostbusted files.
--
main :: IO ()
main = displayConsoleRegions $ do
  argv  <- getArgs
  --
  let
      parse :: [String] -> Options -> Options
      parse ("-h":_)         _ = error helpMessage
      parse ("-p":glob:rest) o = parse rest (o { globPattern = Just glob })
      parse [csv]            o = o { inputCSV = csv }
      parse [csv,out]        o = o { inputCSV = csv , outputDir = out }
      parse _                _ = error helpMessage

      opts      = parse argv defaultOptions

      inDir     = takeDirectory (inputCSV opts)
      outDir    = outputDir opts
      outCSV    = outDir </> "ghostbust_result.csv"
  --
  sayIO $ printf "Input file: %s" (inputCSV opts)
  sayIO $ printf "Input directory: %s" inDir
  sayIO $ printf "Output directory: %s" outDir
  sayIO $ printf "Glob pattern: %s" (show (globPattern opts))

  -- Read in the statistics file from the previous step
  --
  statsfile     <- BL.readFile (inputCSV opts)
  stats         <-
    case CSV.decodeByName statsfile of
      Left err    -> error $ printf "Failed to decode stats file '%s'\nMessage: %s" (inputCSV opts) err
      Right (_,v) -> return (v :: Vector CC.Stats)

  let stats'    =  maybe stats (\pat -> (V.filter (\s -> CC.fileName s ~~ pat)) stats) (globPattern opts)

  -- Try to compile all of the ghostbusted files. The results file is
  -- written as we go.
  --
  createDirectoryIfMissing True outDir
  withSharedFile outCSV WriteMode $ \sh -> do
    -- Write the file header
    withSharedHandle sh $ \h -> do
      B.hPutStr h . B.intercalate "," . V.toList $ CSV.headerOrder (undefined :: Result)
      B.hPutStr h "\n"

    -- Run all tests in parallel, collecting the results. Each thread
    -- writes its result as it goes as well, just in case something goes
    -- wrong.
    res <- testAllP opts stats' sh

    let v = V.sum (V.map variants res)
        s = V.sum (V.map successes res)
        p = fromInteger s / fromInteger v * 100.0 :: Double

    sayIO $ printf ""
    sayIO $ printf "COMPLETE: "
    sayIO $ printf "  Files     : %d" (V.length res)
    sayIO $ printf "  Variants  : %d" v
    sayIO $ printf "  Successes : %d (%.2f%%)" s p


-- Test all Ghostbusted files, in parallel if possible.
--
testAllP :: Options -> Vector Stats -> SharedHandle -> IO (Vector Result)
testAllP opts stats sh = do
  progress <- newProgressBar def { pgTotal = toInteger (V.length stats) }
  numCap <- getNumCapabilities
  printf "Running in parallel on %d capabilities\n" numCap
  results  <- runParIO $
    parForM stats $ \s -> do
      r <- test1 opts s
      liftIO $ tick progress
      liftIO $ withSharedHandle sh $ \h -> BL.hPutStr h (CSV.encode [r])
      return r
  --
  closeConsoleRegion (pgRegion progress)
  return results


-- Same implementation as in Control.Monad.Par, but with fixed type
-- signature allowing ParIO.
--
parMapM :: (Traversable t, NFData b, ParFuture future m) => (a -> m b) -> t a -> m (t b)
parMapM f xs = traverse (spawn . f) xs >>= traverse get

parForM :: (Traversable t, NFData b, ParFuture future m) => t a -> (a -> m b) -> m (t b)
parForM xs f = parMapM f xs


-- Attempt to compile all of the ghostbusted CCs generated from the given
-- input file.
--
test1 :: Options -> Stats -> ParIO Result
test1 opts stat = do
  (degen,defs) <- liftIO $ locateFiles opts stat
  let result    = Result { filePath           = CC.fileName stat
                         , variants           = genericLength defs
                         , successes          = 0
                         , degenerateCompiles = False
                         }
      inDir     = takeDirectory (CC.fileName stat)
  --
  progress  <- liftIO $ newProgressBar def
                  { pgTotal  = genericLength defs + 1
                  , pgFormat = printf "%s :percent [:bar] :current/:total (for :elapsed, :eta remaining)" (takeBaseName (CC.fileName stat))
                  }

  -- First, make sure that we can compile the degenerate configuration
  degenok  <- liftIO $ compileFile opts (inDir </> degen)
  liftIO    $ tick progress

  -- Now compile everything else
  !status  <- parForM defs $ \d -> liftIO $ do
                !s <- compileFile opts (inDir </> d)
                      `catch`
                      \e -> do errIO $ printf "Testing '%s' returned error: %s" d (show (e :: SomeException))
                               return False
                tick progress
                return s
  --
  liftIO $ closeConsoleRegion (pgRegion progress)
  return result { successes          = sum [ 1 | True <- status ]
                , degenerateCompiles = degenok
                }


-- Locate the ghostbusted CCs that were generated for a given file.
--
locateFiles :: Options -> Stats -> IO (FilePath, [FilePath])
locateFiles opts stats =
  let
      inRoot            = takeDirectory (inputCSV opts)
      inDir             = inRoot </> base
      (base,name)       = splitFileName (CC.fileName stats)
      pat               = dropExtension name ++ "*ghostbusted*"
      degen             = dropExtension name ++ "degenerate" <.> "hs"
  in
  do
      ls     <- getDirectoryContents inDir
      yes    <- doesFileExist (inDir </> degen)

      unless yes $ error $ printf "could not find degenerate file '%s'\n" (inDir </> degen)

      let busted = filter (~~ pat) ls
      sayIO  $ printf "Found %d busted CCs for '%s'" (length busted) (CC.fileName stats)
      return $ (degen, busted)


{-# NOINLINE ghc #-}
ghc :: FilePath
ghc = unsafePerformIO $ do
  (_,Just h,_,_) <- createProcess (proc "stack" ["exec", "which", "--", "ghc"]) { std_out = CreatePipe }
  path           <- init `fmap` hGetContents h  -- drop trailing '\n'
  yes            <- doesFileExist path
  if yes
     then do sayIO $ printf "Using 'ghc' at path: %s" path
             return path
     else error "could not find ghc"


-- Run GHC on the given file. No object files are generated. The standard
-- output and error logs are written into files in the output directory.
-- Return whether or not the file compiled successfully.
--
compileFile :: Options -> FilePath -> IO Bool
compileFile opts inFile = do
  let
      outFile   = outputDir opts </> inFile
      outDir    = takeDirectory outFile
      inRoot    = takeDirectory (inputCSV opts)
      inFile'   = inRoot </> inFile
  --
  createDirectoryIfMissing True outDir
  withFile (outFile `replaceExtension` "log") WriteMode $ \h -> do
    let
        -- This initially called "stack exec ghc", but that is very slow.
        compile = (proc ghc ["-fno-code", "-odir", "/dev/null", "-hidir", "/dev/null", "-c", inFile'])
                    { std_out = UseHandle h
                    , std_err = UseHandle h
                    }
    --
    (_,_,_,hdl) <- createProcess compile
    status      <- waitForProcess hdl
    case status of
      ExitSuccess       -> return True
      ExitFailure code  -> do errIO $ printf "compileFile '%s' exited with status (%d)" inFile' code
                              return False


-- Debugging support
-- -----------------

sayIO :: String -> IO ()
sayIO msg = outputConcurrent (msg ++ "\n")
-- sayIO _ = return ()

errIO :: String -> IO ()
errIO msg = errorConcurrent (msg ++ "\n")
-- sayIO _ = return ()

