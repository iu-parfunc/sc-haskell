#!/usr/bin/env stack
-- stack --no-system-ghc --verbosity silent --resolver lts-3.8 --install-ghc runghc --package Ghostbuster
-- | This is a shell script that yanks out all of the connected component
-- data declarations (i.e., all the declarations that refer to each other
-- in the same file) and puts them in separate files -- each file is a
-- separate CC of data declarations.

-- | How to call it and what it does:
-- Let's say we have a dir called "testDir" that has some haskell files
-- that we want to look at (which can be arbitrarily nested). In fact,
-- let's say it looks like this:
--
-- testDir/
-- ├── examples
-- │   ├── DataHashMap.hs
-- │   ├── DataMap.hs
-- │   └── MiniFeldspar.hs
-- └── Foo.hs
--
-- Then we can run:
--    ./ConnectedDDefs.hs testDir/
-- which tells the tool to gather all haskell files contained in testDir,
-- find the connected components and then output one of these per file and
-- try all the various erasure setting per CC. After this, it places these
-- files under the dir "output" in testDirs/. We would then get a structure
-- that looks like this:
--
-- testDir/
-- ├── examples
-- │   ├── DataHashMap.hs
-- │   ├── DataMap.hs
-- │   └── MiniFeldspar.hs
-- ├── Foo.hs
-- └── output
--     ├── ghostbust_data.hdata
--     └── testDir/
--         └── examples
--             ├── DataHashMap_1.hs
--             ├── DataMap_1ghostbusted000.hs
--             ├── DataMap_1.hs
--             ├── MiniFeldspar_1ghostbusted000.hs
--             └── MiniFeldspar_1.hs
--
-- Notice how the original directory structure is replicated. Also notice
-- that we have the file "ghostbust_data.hdata" at the top-level of the
-- generated directory. This contains information per-connected component.
--
-- You can also specify the output directory for our generated files,
-- however I haven't tested this that much so it is very likely buggy.

{-
 - TODO:
   - DONE Count number of GADTs/ADTs
   - DONE Collect Stats
   - DONE Output stats to file
   - DONE Run Ghostbuster and output to a file inside a directory
   - DONE Fix directory structure?  Nah can handle it.

   - DONE Stream output lines to file
   - DONE -- Added in ghostbuster.hs: Handle HUGE search spaces, look before you leap.
   - DONE Report how many fail after ambiguity-check (goal: 0)
     - Changed fields around to support this
   - DONE Cauterize DDefs so that we don't have missing constructors

   - Fix CPP: try expanding it first, then fall back to dropping lines on floor.
   - Report final answer for gradual-erasure hypothesis. (??)
   - Make more robust to exceptions

   - Driver: build parallel driver.
   - Driver: set up directories so intermediate files are not in NFS
-}

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# OPTIONS_GHC -Wall #-}

module ConnectedDDefs where -- Used as a library as well.

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString              as DB
import qualified Data.ByteString.Char8        as DBB
import qualified Data.ByteString.Lazy.Char8   as DBLC
import qualified Data.Csv                     as CSV
import           Data.Function
import           Data.Graph
import           Data.List
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Tree
import           Data.Tuple.Utils
import qualified Data.Vector                  as V
import           GHC.Generics
import qualified Ghostbuster                  as G
import qualified Ghostbuster.Parser.Prog      as GPP
import qualified Ghostbuster.Types            as GT
import           Language.Haskell.Exts        as H hiding (name, parse)
import           Language.Haskell.Exts.SrcLoc (noLoc)
import qualified Language.Preprocessor.Cpphs  as CP
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import qualified System.FilePath.Find         as SFF
import           System.IO

data Stats = Stats
  { numADTs                     :: Int          -- Number of ADTs in this file
  , numADTsWithParams           :: Int          -- Number of ADTs in this file with a type variable
  , numGADTs                    :: Int          -- Number of GADTs in this file
  , numGADTsWithParams          :: Int          -- Number of GADTs in this file with a type variable
  , parseSucc                   :: Int          -- These are integers to make it easier to combine
  , parseFailed                 :: Int
  -- , numCCsInFile                :: Int          -- Number of connected components
  , failedAmb                   :: Int          -- Number of failed erasure settings
  , failedGBust                 :: Int          -- Number of failed erasure settings
  , successfulErasures          :: Int          -- Number of successful erasure settings
  , numParamsInCC               :: Int          -- Average number of parameters in this CC
  , actualCCSearchSpace         :: Integer      -- Actual search space size for this CC
  , exploredCCSearchSpace       :: Integer      -- The search space we searched (in the case of truncation)
  , passGADTPredicate           :: Int          -- How many DDefs pass OUR predicate. <= `numGADTs'
  , numGADTtoADT                :: Int          -- How many DDefs went from GADT->ADT in some erasure variant.
  , gradualProp                 :: Int          -- 1 if the CC was consistent with gradual-erasure for all maxima, 0 otherwise
  , fileName                    :: String       -- File name
  }
 deriving (Show, Eq, Ord, Generic)

instance CSV.FromNamedRecord Stats
instance CSV.ToNamedRecord Stats
instance CSV.ToRecord Stats
instance CSV.DefaultOrdered Stats

instance Monoid Stats where
   mempty      = emptyStats
   mappend x y =
     Stats { numADTs               = on (+) numADTs x y
           , numADTsWithParams     = on (+) numADTsWithParams x y
           , numGADTs              = on (+) numGADTs x y
           , numGADTsWithParams    = on (+) numGADTsWithParams x y
           , parseSucc             = on (+) parseSucc x y
           , parseFailed           = on (+) parseFailed x y
           -- , numCCsInFile          = on (+) numCCsInFile x y
           , failedAmb             = on (+) failedAmb x y
           , failedGBust           = on (+) failedGBust x y
           , successfulErasures    = on (+) successfulErasures x y
           , numParamsInCC         = on (+) numParamsInCC x y
           , actualCCSearchSpace   = on (+) actualCCSearchSpace x y
           , exploredCCSearchSpace = on (+) exploredCCSearchSpace x y
           , passGADTPredicate     = on (+) passGADTPredicate x y
           , numGADTtoADT          = on (+) numGADTtoADT x y
           , gradualProp           = on (+) gradualProp  x y -- Add how many CCs satisfied it.
           , fileName              = fileName x ++ ':':fileName y
           }

emptyStats :: Stats
emptyStats =
  Stats
    { numADTs               = 0
    , numADTsWithParams     = 0
    , numGADTs              = 0
    , numGADTsWithParams    = 0
    , parseSucc             = 0
    , parseFailed           = 0
    -- , numCCsInFile          = 0
    , failedAmb             = 0
    , failedGBust           = 0
    , successfulErasures    = 0
    , numParamsInCC         = 0
    , actualCCSearchSpace   = 0
    , exploredCCSearchSpace = 0
    , passGADTPredicate     = 0
    , numGADTtoADT          = 0
    , gradualProp           = 0
    , fileName              = ""
    }

-- | Read in a module and then gather it into a forest of connected components
-- TZ: Treating pairs and arrows as primitive for now
gParseModule
    :: String
    -> IO (Either [(Module, GT.Prog)] String)
gParseModule str = do
  parsed <- parseFileContentsWithMode
    ParseMode { parseFilename         = str
              , baseLanguage          = Haskell2010
              , extensions            = glasgowExts
              , ignoreLanguagePragmas = False
              , ignoreLinePragmas     = True
              , fixities              = Just preludeFixities
              }
    <$> (CP.runCpphs CP.defaultCpphsOptions "" =<< (readFile str))
  case parsed of
    ParseOk (Module a b c _ _ f decls) ->
      let ddefs                 = [ d | d <- decls, isDataDecl d ]
          g                     = makeGraph ddefs
          (graph, lookupF, _)   = graphFromEdges $ map cleanGraph g
          connComps             = components graph

          connNames :: [[Name]]
          connNames             = map (nub . (concatMap (smash . lookupF) . flatten)) connComps

          cDefs         :: [[Decl]]
          knownDefNames :: [[Name]]
          defsAndKnownNames         = map unzip $ map (lookupDDef ddefs) connNames
          (cDefs, knownDefNames)    = unzip defsAndKnownNames

          -- Take the various data definitions and output them to a file
          cauterizedCDefs           = zipWith3 (cauterize (concatMap thd3 g)) cDefs connNames knownDefNames
          ghostbusterDDefs          = map sParseProg cauterizedCDefs

          -- Make this into a module of CC data defs
          modules                   = map (Module a b c Nothing Nothing f) cauterizedCDefs
      in
      return $ Left (zip modules ghostbusterDDefs)
    ParseFailed _ err -> return $ Right $ "ParseFailed: "++show err

builtin :: [Name]
builtin = map Ident [ "Int", "Bool", "Maybe", "Unit"]   -- Ghostbuster.Types.primitiveTypes


cleanGraph :: (a,b,[(c,d)]) -> (a,b,[c])
cleanGraph (a,b,c) = (a,b, map fst c)

cauterize :: [(Name, Int)] -- List of kinding info for all found TyCons
          -> [Decl] -- List of decls
          -> [Name] -- List of Names in this CC
          -> [Name] -- List of Names already defined in this CC
          -> [Decl]
cauterize nameKinds decls total defined = newDecls
  where
    -- Get the names we know
    unknownNames = total \\ (defined ++ builtin)
    -- hacky but whatevs right now
    createStubs = concatMap (\nm -> if elem nm unknownNames
                                    then case lookup nm nameKinds of
                                            Just i -> [(nm,i)]
                                            Nothing -> []
                                    else ([] :: [(Name,Int)])) unknownNames
    stubDecls = [ DataDecl noLoc DataType [] name vars [] []
                | (name, vars) <- map (\(x,y) -> (x, createVars y)) createStubs]
    createVars i = take i $ map (UnkindedVar . Ident . ("a"++) . show) [(0::Int)..]
    newDecls = stubDecls ++ decls

{-[DataDecl (SrcLoc "Test.hs" 1 1) DataType [] (Ident "Foo") [UnkindedVar (Ident "a-}
{-1"),UnkindedVar (Ident "a2"),UnkindedVar (Ident "a3")] [] []]-}
-- | We have to replicate some of the functionality from the parser here,
-- but we _can't_ use the gParseProg from the parser since that expects
-- annotations (and adding annotations is actually harder than doing this)
sParseProg :: [Decl] -> GT.Prog
sParseProg decls = GT.Prog ddefs vdefs expr
 where
  ddefs = [ cvt nm ts cons | DataDecl  _ DataType _ nm ts   ks _ <- decls, let cons = map (GPP.kconsOfQualConDecl ts) ks ]
       ++ [ cvt nm ts cons | GDataDecl _ DataType _ nm ts _ ks _ <- decls, let cons = map GPP.kconsOfGadtDecl ks ]
  vdefs = []
  expr  = GT.VDef "ghostbuster" (GT.ForAll [] (GT.ConTy "()" [])) (GT.EK "()")

  -- If the data declaration has no cases (e.g. it was generated by the
  -- cauterisation pass) then we can't ghostbust anything, so make sure all
  -- type variables are marked as kept.
  cvt nm ts [] = GT.DDef (GPP.fromName nm) (map GPP.kindTyVar ts) [] [] []
  cvt nm ts ks = GT.DDef (GPP.fromName nm) [] (map GPP.kindTyVar ts) [] ks

-- | Gather all of the data declarations for this module
maybeDataDecl :: Decl -> Maybe Decl
maybeDataDecl v =
  case v of
    DataDecl  _ DataType _ _ _ _ _   -> Just v
    GDataDecl _ DataType _ _ _ _ _ _ -> Just v
    _                                -> Nothing

isDataDecl :: Decl -> Bool
isDataDecl = isJust . maybeDataDecl

-- | We don't particularly care about this, but this is the way we get it
-- out of the graph so...
smash :: (a,b,[b]) -> [b]
smash (_, x, y) = x : y

-- | Given a list of decls, and a list of names in this connected
-- component, return all of the decls in this CC
lookupDDef :: [Decl] -> [Name] -> [(Decl, Name)]
lookupDDef decls names = concatMap
  (\x -> let (yes, name) = getName names x
         in if yes
            then [(x,name)]
            else [])
  decls
{-filter (getName names) decls-}

getName :: [Name] -> Decl -> (Bool, Name)
getName names (DataDecl _ _ _ nm _ _ _)    = (elem nm names, nm)
getName names (GDataDecl _ _ _ nm _ _ _ _) = (elem nm names, nm)
getName _     _                            = error "getName: I only know about ADTs and GADTs"

-- [decl, name, [<list of data exprs used>]]
makeGraph :: [Decl] -> [(Decl, Name, [(Name, Int)])]
makeGraph = map calledConstrs

-- | Return all the constructors (or type constructors) that are used in
-- this data declaration
calledConstrs :: Decl -> (Decl, Name, [(Name, Int)])
calledConstrs decl =
  case decl of
    DataDecl  _ DataType _ nm _   contrs _ -> (decl, nm, called nm (concatMap fromConDecl contrs))
    GDataDecl _ DataType _ nm _ _ contrs _ -> (decl, nm, called nm [ typ | GadtDecl _ _ _ typ <- contrs ])
    _                                           -> error "calledConstrs: i only know about ADTs and GADTs"
  where
    called nm = nub
              . filter ((/= nm) . fst)
              . concatMap gatherCalled


-- | Rip out the types from the ConDecl for non-GADTs
fromConDecl :: QualConDecl -> [Type]
fromConDecl (QualConDecl _ _ _ decl) = destruct decl
  where
    destruct (ConDecl _ ltys)           = ltys
    destruct (InfixConDecl atyp _ btyp) = [atyp,  btyp]
    destruct (RecDecl _ ntys)           = map snd ntys

-- | Gather the called constructors from the type
gatherCalled :: Type -> [(Name, Int)]
gatherCalled = go
  where
    go :: Type -> [(Name, Int)]
    go (TyFun a b)              = go a ++ go b
    go (TyVar _)                = []
    go (TyCon c)                = [(nameOfQName c, 0)]
    go (TyParen t)              = go t
    go (TyBang _ t)             = go t
    go (TyTuple _ ts)           = concatMap go ts

    -- Tricky
    go (TyApp t1 t2)            = case go t1 of
                                    []               -> go t2
                                    ((nm,kind):rest) -> ((nm, kind + 1) : rest) ++ go t2
    go (TyList t)               = go t  -- TLM: ???
    go (TyForall Nothing _ t)   = go t  -- TLM: ???
    go (TyInfix l t r)          = (nameOfQName t, 2) : go l ++ go r

    -- go (TyPromoted _)           = []
    go other                    = error $ "gatherCalled: unhandled case: " ++ show other


strOfName :: Name -> String
strOfName (Ident s)  = s
strOfName (Symbol s) = s

nameOfQName :: QName -> Name
nameOfQName qname =
  case qname of
    UnQual n              -> n
    Qual (ModuleName m) n -> Ident (m ++ '.':strOfName n)
    Special x             -> nameOfSpecialCon x

nameOfSpecialCon :: SpecialCon -> Name
nameOfSpecialCon x =
  Ident $ case x of
    UnitCon            -> "Unit"
    ListCon            -> "[]"
    FunCon             -> "->"
    Cons               -> ":"
    TupleCon Boxed n   -> "("  ++ replicate (n-1) ',' ++  ")"
    TupleCon Unboxed n -> "(#" ++ replicate (n-1) ',' ++ "#)"
    UnboxedSingleCon   -> "(# #)"


---------------------------------------------------------------------------
-- Make the tool runable from the command line.
---------------------------------------------------------------------------

-- We feed this guy a package or a directory
-- This then spits out a ghostbust_data.hdata file in the top level
-- resulting directory that is a CSV that contain information on each
-- connected component that we found and the ghostbusting of that
-- component.
main :: IO ()
main = do
  args <- getArgs
  let (curDir, outputDir) = parseInput args

  putStrLn "Reading env var GHOSTBUST_ONLY_GADTS ... "
  onlyGADTs <- maybe True read <$> lookupEnv "GHOSTBUST_ONLY_GADTS"
  putStrLn $ "GHOSTBUST_ONLY_GADTS: " ++ show onlyGADTs
  inputs    <- SFF.find SFF.always (SFF.fileType SFF.==? SFF.RegularFile SFF.&&? SFF.extension SFF.==? ".hs") curDir

  createDirectoryIfMissing True outputDir -- Just in case, but it _should_ be there
  withFile (outputDir </> "ghostbust_data.csv") WriteMode $ \hdl -> do
    DBB.hPutStrLn hdl $ DB.intercalate "," $ V.toList $ CSV.headerOrder (undefined :: Stats)
    mapM_ (outputCCs onlyGADTs hdl outputDir) inputs


parseInput :: [String] -> (String, String)
-- We place our output in the same directory that we started in but in "output"
parseInput [input]         = (input, (takeDirectory . takeDirectory) input </>  "output" </> input)
parseInput [input, output] = (input, output)
parseInput _               = error "argument parse failed: expected one or two args"

-- | Parse the module and return the list of CCs
outputCCs :: Bool -> Handle -> String -> String -> IO Stats
outputCCs onlyGADTs hdl outputBase input =
  go `catch` \e ->
    do putStrLn $ "--------- Haskell exception while working on " ++ input ++ " --------------"
       print (e :: SomeException)
       return $ emptyStats {parseFailed = 1}
  where
  go =
   gParseModule input >>= \res ->
    case res of
      Left yes -> do
        let
            -- Number all the tests
            tests :: [(Integer, Module, GT.Prog)]
            tests = zipWith (\(m,d) n -> (n,m,d)) yes [1..]

        putStrLn $ "--------- Reading File " ++ input ++ " --------------"
        fmap mconcat $ forM tests $ \(n, mdl, prog) ->
            let
                Module _ _ _ _ _ _ decls = mdl
                ccName          = dropExtension input ++ "_" ++ show n <.> "hs"
                gbName          = outputBase </> dropExtension ccName ++ "ghostbusted" <.> "hs"
                degenName       = outputBase </> dropExtension ccName ++ "degenerate" <.> "hs"
                hasGADTs        = or [True | GDataDecl{} <- decls ]
                doit            = (onlyGADTs && hasGADTs) || (not onlyGADTs && not hasGADTs)
            in
            if doit
            then do
              -- Output the file that we're looking at
              writeModule (outputBase </> ccName) mdl

              -- Also output the generate program, where all type variables
              -- are kept. If this program doesn't compile, there is no
              -- point testing the variants.
              G.writeProg degenName
                $ prog { GT.prgDefs = [ ddef { GT.kVars = kVars ++ cVars ++ sVars
                                             , GT.cVars = []
                                             , GT.sVars = []
                                             }
                                      | ddef@GT.DDef{..} <- GT.prgDefs prog ]}

              -- Fuzz-test this module
              -- fuzz <- G.fuzzTestProg True prog gbName
              fuzz <- G.surveyFuzzTest prog gbName

              let verifyGrad = G.verifyGradualErasure fuzz
              case verifyGrad of
                (n,Nothing) -> putStrLn $ "Woo!  Gradual erasure property held.  NumMaxima: "++show n
                (_,Just s)  -> putStrLn $ "WARNING: Problems with gradual erasure:\n"++take 1000 s

              -- Compute statistics for fuzz-testing this module and save
              let stat = gatherFuzzStats fuzz mdl prog ccName
                  stat' = stat { gradualProp = if Nothing == snd verifyGrad
                                               then 1 else 0 }

              DBLC.hPutStr hdl (CSV.encode [stat'])
              return stat'
            else
              return mempty
      --
      Right err -> do
        putStrLn $ "--------- Failed parse of file " ++ input ++ " --------------"
        putStrLn err
        return $ emptyStats { parseFailed = 1 }


gatherFuzzStats
    :: G.SurveyResult -- [G.FuzzResult (Int,FilePath)]
    -> Module
    -> GT.Prog
    -> FilePath
    -> Stats
gatherFuzzStats G.SurveyResult{..} (Module _ _ _ _ _ _ decls) (GT.Prog ddefs _ _) file =
  let
      res                   = M.elems results
      --
      codeGend              = sum [1 | G.Success{} <- res]
      ambFailed             = sum [1 | G.AmbFailure <- res]
      codeGenFailed         = sum [1 | G.CodeGenFailure <- res]
      ccNumADTs             = sum [1 | DataDecl{} <- decls]
      -- We only cauterize with ADTs
      numADTsCauterized     = sum [1 | DataDecl loc _ _ _ _ _ _ <- decls, loc == noLoc]
      ccNumGADTs            = sum [1 | GDataDecl{} <- decls]
      numADTsWParams        = sum [1 | DataDecl loc _ _ _ tvs _ _   <- decls, loc /= noLoc, not (null tvs)]
      numGADTsWParams       = sum [1 | GDataDecl _  _ _ _ tvs _ _ _ <- decls, not (null tvs)]
      totalParamsInCC       = sum [length tvs | GDataDecl _  _ _ _ tvs _ _ _ <- decls]
                            + sum [length tvs | DataDecl loc _ _ _ tvs _ _   <- decls, loc /= noLoc]
      (searchSpaceSize,actualSearchSpaceSize) =
        case surveyMode of
          G.Exhaustive x                           -> (x,x)
          G.Partial {searchSpace,exploredVariants} -> (searchSpace,exploredVariants)
          G.Greedy{}                               -> error "TODO gatherFuzzStats: greedy search"
  in
  Stats
    { numADTs               = ccNumADTs - numADTsCauterized -- don't count cauterized data decls
    , numGADTs              = ccNumGADTs
    , numADTsWithParams     = numADTsWParams
    , numGADTsWithParams    = numGADTsWParams
    , parseSucc             = 1
    , parseFailed           = 0
    -- , numCCsInFile          = length mods -- Superfluous?
    , failedAmb             = ambFailed
    , failedGBust           = codeGenFailed
    , successfulErasures    = codeGend
    , fileName              = file
    , numParamsInCC         = totalParamsInCC
    , actualCCSearchSpace   = searchSpaceSize
    , exploredCCSearchSpace = actualSearchSpaceSize
    , passGADTPredicate     = length $ filter G.isGADT ddefs
    , numGADTtoADT          = length gadtsBecameASTS
    , gradualProp           = 0  -- This is set later.
    }


-- | Write the decls out to a file
writeModule :: String -> Module -> IO ()
writeModule filename (Module a _ c d e f decls) = do
  createDirectoryIfMissing True (takeDirectory filename)
  writeFile filename (prettyPrint (Module a nm' c d e f decls))
  where
    nm' = ModuleName (takeBaseName filename)

parse :: [String] -> (String, String)
parse [input]         = (input, takeDirectory input </> "output")
parse [input, output] = (input, output)
parse _               = error "parse failed"

exit :: IO a
exit = exitSuccess

usage :: IO ()
usage = do
  version
  putStrLn ""
  putStrLn "Usage: getConnectedComponents [-vh] <inputFile> [<outputFile>]"

version :: IO ()
version = putStrLn "getConnectedComponents version 0.1"
